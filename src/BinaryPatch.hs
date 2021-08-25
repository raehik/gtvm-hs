-- | Simple "linear" (forwards-only) bytestream patcher.
--
-- There is some work done on abstracting over the patch & stream type. I wonder
-- how far I can take it?

module BinaryPatch
  (
  -- * Core patch algorithm
    patch
  , PatchScript
  , Replacement(..)
  , ReplacementMeta(..)
  , Cfg(..)
  , Error(..)

  -- * Patchscript generation
  , genPatchScript
  , Patch(..)
  , ErrorGen(..)
  ) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Builder    as BB
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.List                  ( sortBy )
import qualified Data.List.NonEmpty         as NE
import           Data.List.NonEmpty         ( NonEmpty(..) )

type Bytes = BS.ByteString

-- | A list of "skip x bytes, write bytestring y" actions.
--
-- A patch can be applied to any forward-seeking 'Word8' stream.
type PatchScript a = [(Int, Replacement a)]

-- | A single bytestring replacement.
--
-- Replacements may store extra metadata that can be used at patch time to
-- validate input data (i.e. patching correct file).
data Replacement a = Replacement a (ReplacementMeta a)
    deriving (Eq, Show)

-- | Optional patch-time data for a replacement.
--
-- TODO: for ultra power, mValidate :: Maybe (a -> Maybe UserError)
data ReplacementMeta a = ReplacementMeta
  { rmNullTerminates :: Maybe Int
  -- ^ Stream segment should be null bytes (0x00) only from this index onwards.
  --
  , rmExpected       :: Maybe a
  -- ^ Stream segment should be this.
  } deriving (Eq, Show, Functor)

-- | Patch time config.
data Cfg = Cfg
  { cfgWarnIfLikelyReprocessing :: Bool
  -- ^ If we determine that we're repatching an already-patched stream, continue
  --   with a warning instead of failing.
  , cfgAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show)

-- | Errors encountered during patch time.
--
-- TODO, needs more data inside it.
data Error
  = ErrorPatchOverlong
  | ErrorPatchUnexpectedNonnull
  | ErrorPatchDidNotMatchExpected Bytes Bytes
    deriving (Eq, Show)

-- | Purely run a 'PatchScript' on a 'Data.ByteString.ByteString'.
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
patch :: MonadReader Cfg m => PatchScript Bytes -> Bytes -> m (Either Error Bytes)
patch x1 x2 = go x2 mempty x1
  where
    go :: MonadReader Cfg m
       => Bytes -> BB.Builder -> PatchScript Bytes -> m (Either Error Bytes)
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

    checkExpected bsActual = \case
      Nothing -> return Nothing
      Just bsExpected ->
        asks cfgAllowPartialExpected >>= \case
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

--------------------------------------------------------------------------------

-- | Write the given data into the given offset.
data Patch a = Patch a Int (ReplacementMeta a)
    deriving (Eq, Show)

-- | Error encountered during patchscript generation.
data ErrorGen a
  = ErrorGenOverlap (Patch a) (Patch a)
  -- ^ Two patches wrote to the same offset.
  --
  -- TODO: we could allow this e.g. by selecting one replacement that "wins"
  -- (likely via user annotation) and rewriting the other one to remove the
  -- collision.
    deriving (Eq, Show)

genPatchScript :: [Patch Bytes] -> Either (NonEmpty (ErrorGen Bytes)) (PatchScript Bytes)
genPatchScript pList =
    let pList'                  = sortBy comparePatchOffsets pList
        (_, script, mErrors, _) = execState (go pList') (0, [], Nothing, undefined)
        -- I believe the undefined is inaccessible providing the first patch has
        -- a non-negative offset (negative offsets are forbidden)
     in case mErrors of
          Nothing     -> Right (reverse script)
          Just errors -> Left (NE.reverse errors)
  where
    comparePatchOffsets (Patch _ o1 _) (Patch _ o2 _) = compare o1 o2
    go :: (MonadState (Int, PatchScript Bytes, Maybe (NonEmpty (ErrorGen Bytes)), Patch Bytes) m) => [Patch Bytes] -> m ()
    go [] = return ()
    go (p@(Patch bs o meta):ps) = do
        (cursor, script, mErrors, prevPatch) <- get
        case trySkipTo o cursor of
          -- next offset is behind current cursor: overlapping patches
          -- record error, recover via dropping patch
          Left _ -> do
            let e = ErrorGenOverlap p prevPatch
                errors = case mErrors of
                           Nothing     -> e :| []
                           Just es -> NE.cons e es
            put (cursor, script, Just errors, p)
            go ps
          Right skip -> do
            let cursor' = cursor + skip + BS.length bs
                r       = Replacement bs meta
            put (cursor', (skip, r):script, mErrors, p)
            go ps
    trySkipTo to from =
        let diff = to - from in if diff >= 0 then Right diff else Left (-diff)
