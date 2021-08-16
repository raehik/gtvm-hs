-- | Small interface on top of the linear binary patch algorithm.
module BinaryPatch.Pretty where

import qualified BinaryPatch            as BP

import qualified Data.ByteString        as BS
import qualified Data.Text.Encoding     as Text
import           Data.Text              ( Text )
import           Data.Maybe             ( fromMaybe )
import           GHC.Generics           ( Generic )
import           HexByteString

-- | A single patch applied at a list of offsets, parameterized by patch type.
data MultiPatch a = MultiPatch
  { mpContents    :: a
  -- ^ The value to patch in. Likely a bytestring or text for simple uses.
  , mpOffsets     :: [Offset a]
  } deriving (Eq, Show, Generic)

-- | An offset in a stream, with metadata about it to use when preparing the
--   patch and at patch time.
--
-- Meta is 'Maybe'd solely for an easier Aeson experience. Grrrr.
data Offset a = Offset
  { oOffset :: Int
  -- ^ Stream offset to patch at.
  , oMeta :: Maybe (OffsetMeta a)
  } deriving (Eq, Show, Generic)

data OffsetMeta a = OffsetMeta
  { omMaxLength      :: Maybe Int
  -- ^ Maximum bytestring length allowed to patch in at this offset.
  -- TODO: use single range/span instead (default 0->x, also allow y->x)
  , omNullTerminates :: Maybe Int
  , omExpected       :: Maybe a
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

normalize :: ToBinPatch a => [MultiPatch a] -> Maybe [BP.Patch]
normalize xs = concat <$> mapM go xs
  where
    go (MultiPatch contents os) = mapM (tryMakeSingleReplace contents) os

tryMakeSingleReplace :: ToBinPatch a => a -> Offset a -> Maybe BP.Patch
tryMakeSingleReplace contents (Offset addr mometa) =
    case omMaxLength ometa of
      Just len -> if BS.length bs > len then Nothing else go
      Nothing  -> go
  where
    bs = toBinPatch contents
    ometa = fromMaybe offsetMetaDefault mometa
    rmeta = offsetMetaToReplacementMeta ometa
    go = Just (BP.Patch bs addr rmeta)
    offsetMetaDefault = OffsetMeta Nothing Nothing Nothing

offsetMetaToReplacementMeta :: ToBinPatch a => OffsetMeta a -> BP.ReplacementMeta
offsetMetaToReplacementMeta o = BP.ReplacementMeta
  { BP.rmNullTerminates = omNullTerminates o
  , BP.rmExpected       = toBinPatch <$> omExpected o
  }

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
