module LinearPatch
  ( normalize
  , ReplaceMany(..)
  , Offset(..)
  , ErrorNormalize(..)
  ) where

import           LinearPatch.Patch

import qualified Data.ByteString         as BS
import           Control.Monad.State
import           Data.List               ( sortBy )
import           Data.Maybe              ( fromMaybe )

import           GHC.Generics            ( Generic )

type Bytes = BS.ByteString

-- | Write the given bytestring into the given offsets.
--
-- Intended as a JSON intermediate type, so includes an "expected length" field
-- that is quickly compared with actual length and thrown out.
data ReplaceMany = ReplaceMany
  { rsBytes       :: Bytes
  , rsExpectedLen :: Maybe Int
  , rsOffsets     :: [Offset]
  } deriving (Eq, Show, Generic)

-- | An offset in a stream, with metadata about it to use when preparing the
--   patch and at patch time.
--
-- Intended as a JSON intermediate type, so the meta field is "double" optional.
data Offset = Offset
  { offsetAddress    :: Int
  , offsetMaxLength  :: Maybe Int
  , offsetMeta       :: Maybe ReplacementMeta
  } deriving (Eq, Show, Generic)

-- | Write the given bytestring into the given offset.
data Replace1 = Replace1 Bytes Int ReplacementMeta
    deriving (Eq, Show, Generic)

-- | Errors encountered during patch script generation.
data ErrorNormalize
  = ErrorNormalizeClobber
  -- ^ Two replacements overwrote the same byte(s).
  --
  -- TODO: we could allow this e.g. by selecting one replacement that "wins"
  -- (likely via user annotation) and rewriting the other one to remove the
  -- collision.

  | ErrorNormalizeReplacement' ErrorNormalizeReplacement
    deriving (Eq, Show, Generic)

-- | Errors encountered during patch script generation, related to a single
--   replacement.
data ErrorNormalizeReplacement
  = ErrorNormalizeReplacementNotLengthSpecified
  | ErrorNormalizeReplacementTooLongForOffset
    deriving (Eq, Show, Generic)

-- | Rewrite a list of multi-replacements (one string to many offsets) to a list
--   of individual replacements.
--
-- In essence, it's patch script compilation. Or normalizing.
--
-- TODO do mapMs stop on first failure? (hoping so)
-- TODO provide [ErrorNormalize] error list! instead of just one
normalize :: [ReplaceMany] -> Either ErrorNormalize PatchScript
normalize urs =
    case mapM concatReplaceMany urs of
      Left err  -> Left (ErrorNormalizeReplacement' err)
      Right r1s ->
        let r1sOrdered = sortBy sortReplace1 (concat r1s)
         in case execGo r1sOrdered of
          Nothing -> Left ErrorNormalizeClobber
          Just rs -> return rs
  where
    execGo :: [Replace1] -> Maybe PatchScript
    execGo rs = evalState (go rs) (0, [])
    sortReplace1 (Replace1 _ o1 _) (Replace1 _ o2 _) = compare o1 o2
    go :: (MonadState (Int, PatchScript) m) => [Replace1] -> m (Maybe PatchScript)
    go ((Replace1 bs o meta):r1s) = do
        (cursor, rs) <- get
        case trySkipTo o cursor of
          Nothing -> return Nothing -- clobbered
          Just skip -> do
            let cursor' = cursor + skip + BS.length bs
                r       = Replacement bs meta
            put (cursor', (skip, r):rs)
            go r1s
    go [] = do
        (_, rs) <- get
        return (Just (reverse rs))
    trySkipTo to from =
        let diff = to - from in if diff >= 0 then Just diff else Nothing

concatReplaceMany :: ReplaceMany -> Either ErrorNormalizeReplacement [Replace1]
concatReplaceMany (ReplaceMany bs mLen os) =
    case mLen of
      Just len -> if   BS.length bs /= len
                  then Left ErrorNormalizeReplacementNotLengthSpecified
                  else go
      Nothing -> go
  where
    go =
        case mapM (tryMakeSingleReplace bs) os of
          Just r1s -> Right r1s
          Nothing -> Left ErrorNormalizeReplacementTooLongForOffset

tryMakeSingleReplace :: Bytes -> Offset -> Maybe Replace1
tryMakeSingleReplace bs (Offset addr mLen mMeta) =
    case mLen of
      Just len -> if   BS.length bs > len
                  then Nothing
                  else go
      Nothing -> go
  where
    go = Just (Replace1 bs addr meta)
    meta = fromMaybe replaceMetaDef mMeta
    replaceMetaDef = ReplacementMeta Nothing Nothing
