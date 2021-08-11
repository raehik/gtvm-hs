module LinearPatch.LinearPatchExtra
  ( prepare
  , ReplaceMany(..)
  , Offset(..)
  , ErrorPrep(..)
  ) where

import           LinearPatch

import qualified Data.ByteString        as BS
import           Control.Monad.State
import           Data.List              ( sortBy )
import           Data.Maybe             ( fromMaybe )
import           HexByteString
import qualified Data.Aeson             as Aeson
import           Data.Aeson             ( ToJSON, FromJSON, genericToJSON, genericToEncoding, genericParseJSON )
import           GHC.Generics           ( Generic )
import qualified Numeric                as Numeric

type Bytes = BS.ByteString

jsonCfgCamelDropX :: Int -> Aeson.Options
jsonCfgCamelDropX x = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop x
  , Aeson.rejectUnknownFields = True }

-- | Wrapper over Int that prefers displaying as a hex 0x123ABC value.
newtype HexInt = HexInt { unHexInt :: Int }
    deriving (Eq)

instance Show HexInt where
    show x = Numeric.showHex x ""

-- | Write the given bytestring into the given offsets.
data ReplaceMany = ReplaceMany
  { rsBytes       :: HexByteString
  , rsExpectedLen :: Maybe HexInt
  , rsOffsets     :: [Offset]
  } deriving (Eq, Show, Generic)

jsonCfgReplaceMany :: Aeson.Options
jsonCfgReplaceMany = jsonCfgCamelDropX 2

instance ToJSON   ReplaceMany where
    toJSON     = genericToJSON     jsonCfgReplaceMany
    toEncoding = genericToEncoding jsonCfgReplaceMany
instance FromJSON ReplaceMany where
    parseJSON  = genericParseJSON  jsonCfgReplaceMany

-- | An offset in a stream, with metadata about it to use when preparing the
--   patch and at patch time.
data Offset = Offset
  { offsetAddress    :: HexInt
  , offsetMaxLength  :: Maybe HexInt
  , offsetMeta       :: Maybe ReplacementMeta'
  } deriving (Eq, Show, Generic)

jsonCfgOffset :: Aeson.Options
jsonCfgOffset = jsonCfgCamelDropX 6

instance ToJSON   Offset where
    toJSON     = genericToJSON     jsonCfgOffset
    toEncoding = genericToEncoding jsonCfgOffset
instance FromJSON Offset where
    parseJSON  = genericParseJSON  jsonCfgOffset

-- | Copy that uses our newtypes instead.
data ReplacementMeta' = ReplacementMeta'
  { rm'NullTerminates :: Maybe HexInt
  , rm'Expected       :: Maybe HexByteString
  } deriving (Eq, Show, Generic)

jsonCfgReplacementMeta' :: Aeson.Options
jsonCfgReplacementMeta' = jsonCfgCamelDropX 2

instance ToJSON   ReplacementMeta' where
    toJSON     = genericToJSON     jsonCfgReplacementMeta'
    toEncoding = genericToEncoding jsonCfgReplacementMeta'
instance FromJSON ReplacementMeta' where
    parseJSON  = genericParseJSON  jsonCfgReplacementMeta'

-- | Write the given bytestring into the given offset.
data Replace1 = Replace1 HexByteString HexInt ReplacementMeta'
    deriving (Eq, Show, Generic)

-- | Errors encountered during patch preparation.
data ErrorPrep
  = ErrorPrepOverlap
  -- ^ Two replacements overwrote the same byte(s).
  --
  -- TODO: we could allow this e.g. by selecting one replacement that "wins"
  -- (likely via user annotation) and rewriting the other one to remove the
  -- collision.

  | ErrorPrepReplacement' ErrorPrepReplacement
    deriving (Eq, Show, Generic)

-- | Errors encountered during patch script generation, related to a single
--   replacement.
data ErrorPrepReplacement
  = ErrorPrepReplacementNotLengthSpecified
  | ErrorPrepReplacementTooLongForOffset
    deriving (Eq, Show, Generic)

-- | Rewrite a list of multi-replacements (one string to many offsets) to a list
--   of individual replacements.
--
-- In essence, it's patch script compilation. Or normalizing.
--
-- TODO do mapMs stop on first failure? (hoping so)
-- TODO provide [ErrorPrep] error list! instead of just one
prepare :: [ReplaceMany] -> Either ErrorPrep Patch
prepare = flip evalState 0 $ \rs ->

    case mapM concatReplaceMany urs of
      Left err  -> Left (ErrorPrepReplacement' err)
      Right r1s ->
        let r1sOrdered = sortBy sortReplace1 (concat r1s)
         in case execGo r1sOrdered of
          Nothing -> Left ErrorPrepOverlap
          Just rs -> return rs
  where
    execGo :: [Replace1] -> Maybe Patch
    execGo rs = evalState (go rs) (0, [])
    sortReplace1 (Replace1 _ o1 _) (Replace1 _ o2 _) = compare o1 o2
    go :: (MonadState (Int, Patch) m) => [Replace1] -> m (Maybe Patch)
    go ((Replace1 bs o meta):r1s) = do
        (cursor, rs) <- get
        case trySkipTo o cursor of
          Nothing -> return Nothing -- overlapping replacements
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

-- | Turn a replacement on a list of offsets into a list of @(replacement,
--   offset)@s.
concatReplaces :: MonadState Int m => ReplaceMany -> m (Either ((HexByteString, Int), Int, Int) [Replace1])
concatReplaces (ReplaceMany hBs mHLen os) =
    case mHLen of
      Just hLen ->
        let lenExpected = BS.length (unHexByteString hBs)
            lenActual   = unHexInt hLen
         in if   lenExpected /= lenActual
            then do i <- get
                    return $ Left ((hBs, i), HexInt lenExpected, hLen)
            else go
      Nothing -> go
  where
    go = return $ Right map (Replace1 hBs) os

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
