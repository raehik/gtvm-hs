-- | Small interface on top of the linear binary patch algorithm.
module BinaryPatch.Pretty
  ( MultiPatches(..)
  , MultiPatch(..)
  , Offset(..)
  , Cfg(..)
  , PatchType(..)
  , applyBaseOffset
  , listAlgebraConcatEtc
  , normalize
  , ToBinPatch(..)

  , BP.ReplacementMeta(..)
  ) where

import qualified BinaryPatch            as BP

import qualified Data.ByteString        as BS
import qualified Data.Text.Encoding     as Text
import           Data.Text              ( Text )
import           Data.Maybe             ( fromMaybe )
import           GHC.Generics           ( Generic )
import           HexByteString
import           Util

type Bytes = BS.ByteString

-- | A list of patches sharing a configuration, each applied at a list of
--   offsets, abstracted over patch type.
data MultiPatches a = MultiPatches
  { mpsBaseOffset :: Maybe Int
  -- ^ The base offset from which all offsets are located. Subtracted from each
  --   offset value to obtain the actual offset. Any offset located before the
  --   base offset (x where x < base) is discarded as erroneous.
  --
  --   This feature enables us to allow negative offsets. For example, say you
  --   set the base offset to @-10@. This is equivalent to stating that every
  --   offset in the list is to be shifted +10 bytes. Thus, all offsets x where
  --   x >= -10 are now valid.
  --
  --   The original rationale behind this feature was to ease assembly patches
  --   on ELFs. Decompilers apparently don't like giving you physical file
  --   offsets, only virtual addresses. However, the physical file offset can be
  --   recovered via the following steps:
  --
  --     1.      add the containing ELF segment's physical offset
  --     2. subtract the containing ELF segment's virtual address
  --
  --   Now by doing that once for every segment you're interested in, you can
  --   use your decompiler addresses instead of doing the maths for each patch
  --   manually! And you can use the same safety checks as always to ensure
  --   correctness.
  , mpsPatches :: [MultiPatch a]
  } deriving (Eq, Show, Generic)

-- | A single patch applied at a list of offsets, parameterized by patch type.
data MultiPatch a = MultiPatch
  { mpContents    :: a
  -- ^ The value to patch in. Likely a bytestring or text for simple uses.
  , mpOffsets     :: [Offset a]
  } deriving (Eq, Show, Generic)

-- | An offset in a stream, with metadata about it to use when preparing the
--   patch and at patch time.
data Offset a = Offset
  { oOffset         :: Int
  -- ^ Stream offset to patch at.

  , oAbsoluteOffset :: Maybe Int
  -- ^ Absolute stream offset to patch at. Compared with actual offset
  --   (calculated from offset and base offset).

  , oMaxLength      :: Maybe Int
  -- ^ Maximum bytestring length allowed to patch in at this offset.
  -- TODO: use single range/span instead (default 0->x, also allow y->x)

  , oPatchMeta      :: Maybe (BP.ReplacementMeta a)
  -- ^ Patch-time info for the replacement at this offset.
  --
  -- Named "patch meta" instead of the more correct "replacement meta" for more
  -- friendly JSON field naming. We wrap it in a 'Maybe' for similar reasons,
  -- plus it means the default can be inserted later on.
  } deriving (Eq, Show, Generic)

data Cfg = Cfg
  { cfgPatchType :: PatchType
  } deriving (Eq, Show)

-- | Indicates the type of patch being done. Both binary and text patches are
--   "compiled" down to simpler binary patches, but by remembering the original
--   format, we can attempt to provide better error behaviour.
data PatchType
  = PatchTypeBinary
  -- ^ Binary patch: show errors etc. using binary-friendly hex bytes
  | PatchTypeUTF8
  -- ^ Text (UTF-8) patch: show errors etc. using text (UTF-8)
    deriving (Eq, Show)

-- Drops no info, not easy to consume.
applyBaseOffset :: [MultiPatches a] -> [(Int, [(MultiPatch a, [Offset a])])]
applyBaseOffset = map go
  where
    go :: MultiPatches a -> (Int, [(MultiPatch a, [Offset a])])
    go mps = (baseOffset, recalculateMultiPatchOffsets baseOffset (mpsPatches mps))
      where
        baseOffset = fromMaybe 0 (mpsBaseOffset mps)

-- lmao this sucks. generalisation bad
listAlgebraConcatEtc :: [(a, [(b, [c])])] -> ([b], [(c, a)])
listAlgebraConcatEtc = mconcat . map go
  where
    go (baseOffset, inps) = tuplemconcat (map (go' baseOffset) inps)
    go' x (mp, offs) = (mp, map (\o -> (o, x)) offs)
    tuplemconcat = foldr (\(a, bs) (as, bs') -> (a:as, bs <> bs')) ([], mempty)

recalculateMultiPatchOffsets :: Int -> [MultiPatch a] -> [(MultiPatch a, [Offset a])]
recalculateMultiPatchOffsets baseOffset = map go
  where
    go :: MultiPatch a -> (MultiPatch a, [Offset a])
    go mp =
        let (osRecalculated, osInvalid) = recalculateOffsets baseOffset (mpOffsets mp)
         in (mp { mpOffsets = osRecalculated }, osInvalid)

recalculateOffsets :: Int -> [Offset a] -> ([Offset a], [Offset a])
recalculateOffsets baseOffset = partitionMaybe go
  where
    go o = if actualOffset >= 0 then Just (o { oOffset = actualOffset }) else Nothing
      where actualOffset = oOffset o - baseOffset

normalize :: ToBinPatch a => [MultiPatch a] -> Maybe [BP.Patch Bytes]
normalize xs = concat <$> mapM go xs
  where
    go (MultiPatch contents os) = mapM (tryMakeSingleReplace contents) os

tryMakeSingleReplace :: ToBinPatch a => a -> Offset a -> Maybe (BP.Patch Bytes)
tryMakeSingleReplace contents (Offset os maos maxLen mMeta) =
    if offsetIsCorrect
    then case maxLen of
           Just len -> if BS.length bs > len then Nothing else go
           Nothing  -> go
    else Nothing
  where
    go = Just (BP.Patch bs os metaBin)
    metaBin = fmap toBinPatch meta
    bs = toBinPatch contents
    meta = fromMaybe metaDefault mMeta
    metaDefault = BP.ReplacementMeta Nothing Nothing
    offsetIsCorrect = case maos of
      Nothing -> True
      Just aos -> os == aos

-- | How to turn a given type into a binary patch.
class ToBinPatch a where
    toBinPatch :: a -> BS.ByteString

-- | Bytestrings are copied as-is.
instance ToBinPatch BS.ByteString where
    toBinPatch = id

-- | Text is converted to UTF-8 bytes and null-terminated (!).
instance ToBinPatch Text where
    -- TODO sucks I gotta do a snoc here >:(
    toBinPatch t = BS.snoc (Text.encodeUtf8 t) 0x00

-- | Bytestring wrapper for Aeson.
instance ToBinPatch HexByteString where
    toBinPatch = unHexByteString
