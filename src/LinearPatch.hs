-- | Linear patch algorithm.

module LinearPatch
  ( patch
  , Patch
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

-- | A list of "skip x bytes, write bytestring y" actions.
--
-- A patch can be applied to any forward-seeking 'Word8' stream.
type Patch = [(Int, Replacement)]

-- | A single bytestring replacement.
--
-- Replacements may store extra metadata that can be used at patch time to
-- validate input data (i.e. patching correct file).
data Replacement = Replacement Bytes ReplacementMeta
    deriving (Eq, Show, Generic)

-- | Optional patch time data for a replacement.
--
-- TODO: for ultra power, rmValidate :: Maybe (Bytes -> Maybe UserError)
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
  -- ^ If we determine that we're repatching an already-patched stream, continue
  --   with a warning instead of failing.
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

-- | Purely apply a 'Patch' to a 'Data.ByteString.ByteString'.
--
-- We use 'Data.ByteString.Builder.Builder's so performance shouldn't be
-- garbage.
--
-- An IO-based patcher could be plenty faster and work on much larger files, but
-- while I'm interested in patching relatively small files, a fully pure
-- patching process is leagues simpler to handle.
--
-- TODO: not doing "likely reprocessing" check (too much of a pain)
-- TODO: Lazy ByteStrings might make sense here.
patch :: MonadReader CPatch m => Patch -> Bytes -> m (Either Error Bytes)
patch x1 x2 = go x2 mempty x1
  where
    go :: MonadReader CPatch m
       => Bytes -> BB.Builder -> Patch -> m (Either Error Bytes)
    go bs b = \case
      -- successfully reached end of patch: execute the builder
      [] -> do
        let b' = b <> BB.byteString bs
        return $ Right $ BL.toStrict $ BB.toLazyByteString b'

      -- next replacement in the patch
      (skip, Replacement bsReplace meta) : rs -> do

        -- split stream into 3: before replace, to-replace, after replace
        case extractActual skip (BS.length bsReplace) bs of
          Nothing -> return $ Left ErrorPatchOverlong
          Just (bsBefore, bsActual, bsAfter) ->

            -- if provided, strip trailing nulls from to-replace bytestring
            case tryStripNulls bsActual (rmNullTerminates meta) of
              Nothing -> return $ Left ErrorPatchUnexpectedNonnull
              Just bsActual' ->

                -- if provided, check that the to-replace bytestring matches the
                -- expected one
                checkExpected bsActual' (rmExpected meta) >>= \case
                  Just (bsa, bse) -> return $ Left $ ErrorPatchDidNotMatchExpected bsa bse
                  Nothing ->

                    -- append to the builder and continue with next replacements
                    let b' = b <> BB.byteString bsBefore <> BB.byteString bsReplace
                     in go bsAfter b' rs

    extractActual skip len bs =
        let (bsBefore, bs')     = BS.splitAt skip bs
            (bsActual, bsAfter) = BS.splitAt len  bs'
         in if   BS.length bsActual == len
            then Just (bsBefore, bsActual, bsAfter)
            else Nothing

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
    tryStripNulls bsActual = \case
      Nothing        -> Just bsActual
      Just nullsFrom ->
        let (bsActual', bsNulls) = BS.splitAt nullsFrom bsActual
         in if   bsNulls == BS.replicate (BS.length bsNulls) 0x00
            then Just bsActual'
            else Nothing
