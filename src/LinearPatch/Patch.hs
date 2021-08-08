-- | Linear patch algorithm.

module LinearPatch.Patch
  ( patch
  , PatchScript
  , Replacement(..)
  , ReplacementMeta(..)
  , CPatch(..)
  , Error(..)
  ) where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Builder as BB
import           Control.Monad.Reader

import           GHC.Generics            ( Generic )

type Bytes = BS.ByteString

-- | A compiled patch script.
--
-- A patch script is a list of "skip x bytes, write bytestring y" actions. This
-- can be run against a source bytestream to "patch" it.
type PatchScript = [(Int, Replacement)]

-- | A single bytestring replacement.
--
-- Replacements may store extra information that can be used at patch time to
-- validate input data (i.e. patching correct file).
data Replacement = Replacement Bytes ReplacementMeta
    deriving (Eq, Show, Generic)

-- | Optional patch time data for a replacement.
--
-- TODO: for ultra power, rmValidate :: Maybe (Bytes -> Maybe UserError) where a
-- Just indicates failure to validate.
data ReplacementMeta = ReplacementMeta
  { rmNullTerminates :: Maybe Int
  -- ^ The present bytestring should be null bytes (0x00) only from this index
  -- onwards.
  , rmExpected :: Maybe Bytes
  -- ^ The present bytestring should be this.
  } deriving (Eq, Show, Generic)

-- | Patch time config.
data CPatch = CPatch
  { cPatchWarnIfLikelyReprocessing :: Bool
  -- ^ If likely reprocessing, continue with a warning instead of failing.
  , cPatchAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show, Generic)

-- | Errors encountered during patch time.
--
-- TODO, needs more data inside it.
data Error
  = ErrorPatchOverlong
  | ErrorPatchUnexpectedNonnull
  | ErrorPatchDidNotMatchExpected Bytes Bytes
    deriving (Eq, Show, Generic)

-- TODO: not doing "likely reprocessing" check (too much of a pain)
-- TODO: provide alternate IO solution (for arbitrary-size files)
-- TODO: Lazy ByteStrings might make sense here.
patch :: MonadReader CPatch m => PatchScript -> Bytes -> m (Either Error Bytes)
patch x1 x2 = go x2 mempty x1
  where
    go :: MonadReader CPatch m
       => Bytes -> BB.Builder -> PatchScript -> m (Either Error Bytes)
    go bs b = \case
      [] -> do
        let b' = b <> BB.byteString bs
        return $ Right $ BL.toStrict $ BB.toLazyByteString b'
      (skip, Replacement bsReplace meta) : rs -> do
        let (bsBefore, bsActualAndAfter) = BS.splitAt skip bs
        case trySplitActualAndAfter bsActualAndAfter (BS.length bsReplace) of
          Nothing -> return $ Left ErrorPatchOverlong
          Just (bsActual, bsAfter) -> do
            checkExpected bsActual (rmExpected meta) >>= \case
              Just (bsa, bse) -> return $ Left $ ErrorPatchDidNotMatchExpected bsa bse
              Nothing ->
                checkNullTerminates bsActual (rmNullTerminates meta) >>= \case
                  False -> return $ Left ErrorPatchUnexpectedNonnull
                  True  ->
                    let b' = b <> BB.byteString bsBefore <> BB.byteString bsReplace
                    in go bsAfter b' rs
    trySplitActualAndAfter bsActualAndAfter replLen = do
        let (bsActual, bsAfter) = BS.splitAt replLen bsActualAndAfter
         in if   BS.length bsActual == replLen
            then Just (bsActual, bsAfter)
            else Nothing
    checkExpected bsActual = \case
      Nothing -> return Nothing
      Just bsExpected ->
        asks cPatchAllowPartialExpected >>= \case
          True  ->
            if   BS.isPrefixOf bsActual bsExpected
            then return Nothing
            else return $ Just (bsActual, bsExpected)
          False ->
            if   bsExpected == bsActual
            then return Nothing
            else return $ Just (bsActual, bsExpected)
    checkNullTerminates bsActual = \case
      Nothing        -> return True
      Just nullsFrom ->
        let bsActualNulls = BS.drop nullsFrom bsActual
         in return $ bsActualNulls == BS.replicate (BS.length bsActualNulls) 0x00
