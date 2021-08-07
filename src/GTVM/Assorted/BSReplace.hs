-- TODO: This would be tons of fun to test with QuickCheck.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}

module GTVM.Assorted.BSReplace where

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteString.Builder as BB
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.List ( sortBy )
import           Data.Maybe ( fromMaybe )
import qualified Data.Text.Encoding      as Text
import           Data.Text ( Text )

import qualified Data.Aeson as Aeson
import           Data.Aeson ( ToJSON, FromJSON )
import           GHC.Generics ( Generic )
import           GTVM.Common.Orphans()

type Bytes = BS.ByteString

-- user facing replace object (JSON)
data UserReplace = UserReplace
  { urBytes       :: Bytes
  , urExpectedLen :: (Maybe Int)
  , urOffsets     :: [Offset]
  } deriving (Eq, Show, Generic)

-- user facing offset object (JSON) (meta is optional for that reason)
data Offset = Offset
  { offsetAddress    :: Int
  , offsetMaxLength  :: (Maybe Int)
  , offsetMeta       :: Maybe ReplaceMeta
  } deriving (Eq, Show, Generic)

data ReplaceMeta = ReplaceMeta
  { rmNullTerminates :: Maybe Int
  , rmExpected :: Maybe Bytes
  } deriving (Eq, Show, Generic)

replaceMetaDef :: ReplaceMeta
replaceMetaDef = ReplaceMeta
  { rmNullTerminates = Nothing
  , rmExpected  = Nothing }

jsonCfgUserReplace :: Aeson.Options
jsonCfgUserReplace = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
  , Aeson.rejectUnknownFields = True }

instance ToJSON   UserReplace where
    toJSON     = Aeson.genericToJSON     jsonCfgUserReplace
    toEncoding = Aeson.genericToEncoding jsonCfgUserReplace
instance FromJSON UserReplace where
    parseJSON  = Aeson.genericParseJSON  jsonCfgUserReplace

jsonCfgOffset :: Aeson.Options
jsonCfgOffset = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 6
  , Aeson.rejectUnknownFields = True }

instance ToJSON   Offset where
    toJSON     = Aeson.genericToJSON     jsonCfgOffset
    toEncoding = Aeson.genericToEncoding jsonCfgOffset
instance FromJSON Offset where
    parseJSON  = Aeson.genericParseJSON  jsonCfgOffset

jsonCfgReplaceMeta :: Aeson.Options
jsonCfgReplaceMeta = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
  , Aeson.rejectUnknownFields = True }

instance ToJSON   ReplaceMeta where
    toJSON     = Aeson.genericToJSON     jsonCfgReplaceMeta
    toEncoding = Aeson.genericToEncoding jsonCfgReplaceMeta
instance FromJSON ReplaceMeta where
    parseJSON  = Aeson.genericParseJSON  jsonCfgReplaceMeta

-- (skip, replace) -- skip is +ve only
type ReplaceScript = [(Int, Replace)]

-- replacement bytestring, meta
data Replace = Replace Bytes ReplaceMeta
    deriving (Eq, Show)

-- replacement bytestring, offset, meta
data Replace1 = Replace1 Bytes Int ReplaceMeta
    deriving (Eq, Show)

data ErrNormalise
  = ErrNormaliseReplacement' ErrNormaliseReplacement
  | ErrNormaliseClobber
    deriving (Eq, Show)

data ErrNormaliseReplacement
  = ErrNormaliseReplacementNotLengthSpecified
  | ErrNormaliseReplacementTooLongForOffset
    deriving (Eq, Show)

-- TODO do mapMs stop on first failure? (hoping so)
normalise :: [UserReplace] -> Either ErrNormalise ReplaceScript
normalise urs =
    case mapM expandUserReplace urs of
      Left err  -> Left (ErrNormaliseReplacement' err)
      Right r1s ->
        let r1sOrdered = sortBy sortReplace1 (concat r1s)
         in case execGo r1sOrdered of
          Nothing -> Left ErrNormaliseClobber
          Just rs -> return rs
  where
    execGo :: [Replace1] -> Maybe ReplaceScript
    execGo rs = evalState (go rs) (0, [])
    sortReplace1 (Replace1 _ o1 _) (Replace1 _ o2 _) = compare o1 o2
    go :: (MonadState (Int, ReplaceScript) m) => [Replace1] -> m (Maybe ReplaceScript)
    go ((Replace1 bs o meta):r1s) = do
        (cursor, rs) <- get
        case trySkipTo o cursor of
          Nothing -> return Nothing -- clobbered
          Just skip -> do
            let cursor' = cursor + skip + BS.length bs
                r       = Replace bs meta
            put (cursor', (skip, r):rs)
            go r1s
    go [] = do
        (_, rs) <- get
        return (Just (reverse rs))
    trySkipTo to from =
        let diff = to - from in if diff >= 0 then Just diff else Nothing

expandUserReplace :: UserReplace -> Either ErrNormaliseReplacement [Replace1]
expandUserReplace (UserReplace bs mLen os) =
    case mLen of
      Just len -> if   BS.length bs /= len
                  then Left ErrNormaliseReplacementNotLengthSpecified
                  else go
      Nothing -> go
  where
    go =
        case mapM (tryMakeSingleReplace bs) os of
          Just r1s -> Right r1s
          Nothing -> Left ErrNormaliseReplacementTooLongForOffset

tryMakeSingleReplace :: Bytes -> Offset -> Maybe Replace1
tryMakeSingleReplace bs (Offset addr mLen mMeta) =
    case mLen of
      Just len -> if   BS.length bs > len
                  then Nothing
                  else go
      Nothing -> go
  where
    meta = fromMaybe replaceMetaDef mMeta
    go = Just (Replace1 bs addr meta)

--------------------------------------------------------------------------------

data CReplace = CReplace
  { cReplaceWarnIfLikelyReprocessing :: Bool
  -- ^ If likely reprocessing, continue with a warning instead of failing.
  , cReplaceAllowPartialExpected :: Bool
  -- ^ If enabled, allow partial expected bytes checking. If disabled, then even
  --   if the expected bytes are a prefix of the actual, fail.
  } deriving (Eq, Show)

data ErrReplace
  = ErrReplaceOverlong
  | ErrReplaceUnexpectedNonnull
  | ErrReplaceDidNotMatchExpected
    deriving (Eq, Show)

-- TODO: not doing likely reprocessing check (kind of a pain)
-- TODO: output info in errors
-- TODO: provide alternate IO solution (for arbitrary-size files)
-- TODO: Lazy ByteStrings might make sense here.
replaceBytes :: MonadReader CReplace m => ReplaceScript -> Bytes -> m (Either ErrReplace Bytes)
replaceBytes x1 x2 = go x2 mempty x1
  where
    go :: MonadReader CReplace m
       => Bytes -> BB.Builder -> ReplaceScript -> m (Either ErrReplace Bytes)
    go bs b = \case
      [] -> do
        let b' = b <> BB.byteString bs
        return $ Right $ BL.toStrict $ BB.toLazyByteString b'
      (skip, Replace bsReplace meta) : rs -> do
        let (bsBefore, bsOrigAndAfter) = BS.splitAt skip bs
        case trySplitOrigAndAfter bsOrigAndAfter (BS.length bsReplace) of
          Nothing -> return $ Left ErrReplaceOverlong
          Just (bsOrig, bsAfter) -> do
            checkExpected bsOrig (rmExpected meta) >>= \case
              False -> return $ Left ErrReplaceDidNotMatchExpected
              True  ->
                checkNullTerminates bsOrig (rmNullTerminates meta) >>= \case
                  False -> return $ Left ErrReplaceUnexpectedNonnull
                  True  ->
                    let b' = b <> BB.byteString bsBefore <> BB.byteString bsReplace
                    in go bsAfter b' rs
    trySplitOrigAndAfter bsOrigAndAfter origLen = do
        let (bsOrig, bsAfter) = BS.splitAt origLen bsOrigAndAfter
         in if   BS.length bsOrig == origLen
            then Just (bsOrig, bsAfter)
            else Nothing
    checkExpected bsOrig = \case
      Nothing -> return True
      Just bsExpected ->
        asks cReplaceAllowPartialExpected >>= \case
          True  -> return $ BS.isPrefixOf bsExpected bsOrig
          False -> return $ bsExpected == bsOrig
    checkNullTerminates bsOrig = \case
      Nothing        -> return True
      Just nullsFrom ->
        let bsOrigNulls = BS.drop nullsFrom bsOrig
         in return $ bsOrigNulls == BS.replicate (BS.length bsOrigNulls) 0x00

--------------------------------------------------------------------------------

rEx1ur :: [UserReplace]
rEx1ur = [u b123 Nothing [o 1 Nothing rm', o 5 Nothing rm', o 9 Nothing rm']]
  where
    b123 = BS.pack [0x31, 0x32, 0x33]
    u = UserReplace
    o = Offset
    rm' = Nothing

rEx1bs :: Bytes
rEx1bs = BS.pack [0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C]

rEx1ur2 :: [UserReplace]
rEx1ur2 = [u b123 Nothing [o 1 Nothing rm', o 5 Nothing rm', o 9 Nothing rm']]
  where
    b123 = BS.pack [0x31, 0x32, 0x33, 0x00]
    u = UserReplace
    o = Offset
    rm' = Nothing

--------------------------------------------------------------------------------

-- simplified string replace object (JSON)
data StrReplace = StrReplace
  { srText    :: Text
  , srAddress :: Int
  , srMeta    :: Maybe StrReplaceMeta
  } deriving (Eq, Show, Generic)

data StrReplaceMeta = StrReplaceMeta
  { srmNullTerminates :: Maybe Int
  , srmExpected       :: Maybe Bytes
  , srmMaxLength      :: Maybe Int
  } deriving (Eq, Show, Generic)

jsonCfgStrReplace :: Aeson.Options
jsonCfgStrReplace = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
  , Aeson.rejectUnknownFields = True }

instance ToJSON   StrReplace where
    toJSON     = Aeson.genericToJSON     jsonCfgUserReplace
    toEncoding = Aeson.genericToEncoding jsonCfgUserReplace
instance FromJSON StrReplace where
    parseJSON  = Aeson.genericParseJSON  jsonCfgUserReplace

jsonCfgStrReplaceMeta :: Aeson.Options
jsonCfgStrReplaceMeta = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
  , Aeson.rejectUnknownFields = True }

instance ToJSON   StrReplaceMeta where
    toJSON     = Aeson.genericToJSON     jsonCfgUserReplace
    toEncoding = Aeson.genericToEncoding jsonCfgUserReplace
instance FromJSON StrReplaceMeta where
    parseJSON  = Aeson.genericParseJSON  jsonCfgUserReplace

parseStrReplace :: StrReplace -> UserReplace
parseStrReplace sr = ur
  where
    ur = UserReplace
        { urBytes       = BS.snoc (Text.encodeUtf8 (srText sr)) 0x00
        , urExpectedLen = Nothing
        , urOffsets     = [offset] }
    offset = Offset
        { offsetAddress   = srAddress sr
        , offsetMaxLength = maxLen
        , offsetMeta      = meta }
    (meta, maxLen) =
        case srMeta sr of
          Nothing    -> (Nothing, Nothing)
          Just sMeta ->
            let rm = Just $ ReplaceMeta
                        { rmNullTerminates = srmNullTerminates sMeta
                        , rmExpected       = srmExpected sMeta }
                ml = srmMaxLength sMeta
            in (rm, ml)
