{-# LANGUAGE OverloadedStrings #-}

-- | Intent: A sum type with a constructor corresponding to each SCP macro that
--   stores user-facing text, with fields to allow checking & replacing such
--   text.
--
-- We need sum types if we want to handle everything in one place. CSVs don't
-- support sum types. YAML does. And with sum types, we can also generate
-- comments from a source SCP -- to e.g. say when another SCP is loaded.

module GTVM.SCP.TL where

import GTVM.SCP
import Strongweak

import Raehik.Check

import GHC.Generics ( Generic )

import Data.Aeson qualified as Aeson
import Data.Aeson ( ToJSON(..), FromJSON(..)
                  , genericToJSON, genericToEncoding , genericParseJSON )
import GTVM.Common.Json

import Data.Text ( Text )
import Data.Text qualified as Text
import Data.Map ( Map )
import Data.Map qualified as Map
import Data.Char qualified
import Data.Maybe ( fromMaybe )
import Util ( tshow )

import Data.Yaml.Pretty qualified as Yaml.Pretty

import Control.Monad.State

import Numeric.Natural ( Natural )

type Seg' = Seg 'Weak Text
type SCP' = [Seg']

type SCPTL c a = [TLSeg c a]

data Env = Env
  { envPendingPlaceholder :: Text

  , envSpeakerIDMap       :: Natural -> Maybe Text
  -- ^ Attempt to obtain a pretty speaker name from an ID.
  --
  -- This data isn't stored in the repo, and must instead be parsed at runtime.

  } deriving (Generic)

data TLSeg (c :: Check) a
  = TLSegTextbox'  (TLSegTextbox c a)
  | TLSegChoice'   [TLSegChoice c a]
  | TLSeg22Choice' (TLSeg22 c a)
  | TLSeg35Choice' (TLSegChoice c a)
  | TLSegComment'  TLSegComment
    deriving (Generic)

deriving instance (Eq   (CheckRep c a), Eq   a) => Eq   (TLSeg c a)
deriving instance (Show (CheckRep c a), Show a) => Show (TLSeg c a)

deriving instance Functor     (TLSeg 'CheckEqual)
deriving instance Foldable    (TLSeg 'CheckEqual)
deriving instance Traversable (TLSeg 'CheckEqual)

jcTLSeg :: Aeson.Options
jcTLSeg = Aeson.defaultOptions
  { Aeson.constructorTagModifier = map Data.Char.toLower . init . drop 5
  , Aeson.sumEncoding = Aeson.TaggedObject
    { Aeson.tagFieldName = "type"
    , Aeson.contentsFieldName = "contents" }}

instance (ToJSON   (CheckRep c a), ToJSON   a) => ToJSON   (TLSeg c a) where
    toJSON     = genericToJSON     jcTLSeg
    toEncoding = genericToEncoding jcTLSeg
instance (FromJSON (CheckRep c a), FromJSON a) => FromJSON (TLSeg c a) where
    parseJSON  = genericParseJSON  jcTLSeg

data TLSegComment = TLSegComment
  { scpTLCommentCommentary :: [Text]
  , scpTLCommentMeta       :: Map Text Text
  } deriving stock (Generic, Eq, Show)

jcTLSegComment :: Aeson.Options
jcTLSegComment =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpTLComment" :: String)

instance ToJSON   TLSegComment where
    toJSON     = genericToJSON     jcTLSegComment
    toEncoding = genericToEncoding jcTLSegComment
instance FromJSON TLSegComment where
    parseJSON  = genericParseJSON  jcTLSegComment

data TLSegTextbox (c :: Check) a = TLSegTextbox
  { tlSegTextboxSource      :: CheckRep c a
  , tlSegTextboxTranslation :: a
  , tlSegTextboxOverflow    :: Maybe a
  } deriving (Generic)

deriving instance (Eq   (CheckRep c a), Eq   a) => Eq   (TLSegTextbox c a)
deriving instance (Show (CheckRep c a), Show a) => Show (TLSegTextbox c a)

instance Functor     (TLSegTextbox 'CheckEqual) where
    fmap     f (TLSegTextbox s t o) = TLSegTextbox (f s) (f t) (fmap f o)
instance Foldable    (TLSegTextbox 'CheckEqual) where
    foldMap  f (TLSegTextbox s t o) = f s <> f t <> foldMap f o
instance Traversable (TLSegTextbox 'CheckEqual) where
    traverse f (TLSegTextbox s t o) =
        TLSegTextbox <$> f s <*> f t <*> traverse f o

jcTLSegTextbox :: Aeson.Options
jcTLSegTextbox =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("tlSegTextbox" :: String)

instance (ToJSON   (CheckRep c a), ToJSON   a) => ToJSON   (TLSegTextbox c a) where
    toJSON     = genericToJSON     jcTLSegTextbox
    toEncoding = genericToEncoding jcTLSegTextbox
instance (FromJSON (CheckRep c a), FromJSON a) => FromJSON (TLSegTextbox c a) where
    parseJSON  = genericParseJSON  jcTLSegTextbox

data TLSegChoice (c :: Check) a = TLSegChoice
  { tlSegChoiceSource :: CheckRep c a
  , tlSegChoiceTranslation :: a
  } deriving (Generic)

deriving instance (Eq   (CheckRep c a), Eq   a) => Eq   (TLSegChoice c a)
deriving instance (Show (CheckRep c a), Show a) => Show (TLSegChoice c a)

instance Functor     (TLSegChoice 'CheckEqual) where
    fmap     f (TLSegChoice s t) = TLSegChoice (f s) (f t)
instance Foldable    (TLSegChoice 'CheckEqual) where
    foldMap  f (TLSegChoice s t) = f s <> f t
instance Traversable (TLSegChoice 'CheckEqual) where
    traverse f (TLSegChoice s t) =
        TLSegChoice <$> f s <*> f t

jcTLSegChoice :: Aeson.Options
jcTLSegChoice =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("tlSegChoice" :: String)

instance (ToJSON   (CheckRep c a), ToJSON   a) => ToJSON   (TLSegChoice c a) where
    toJSON     = genericToJSON     jcTLSegChoice
    toEncoding = genericToEncoding jcTLSegChoice
instance (FromJSON (CheckRep c a), FromJSON a) => FromJSON (TLSegChoice c a) where
    parseJSON  = genericParseJSON  jcTLSegChoice

data TLSeg22 (c :: Check) a = TLSeg22
  { tlSeg22TopicSource      :: CheckRep c a
  , tlSeg22TopicTranslation :: a
  , tlSeg22Choices          :: [TLSegChoice c a]
  } deriving (Generic)

deriving instance (Eq   (CheckRep c a), Eq   a) => Eq   (TLSeg22 c a)
deriving instance (Show (CheckRep c a), Show a) => Show (TLSeg22 c a)

instance Functor     (TLSeg22 'CheckEqual) where
    fmap     f (TLSeg22 s t cs) = TLSeg22 (f s) (f t) (fmap (fmap f) cs)
instance Foldable    (TLSeg22 'CheckEqual) where
    foldMap  f (TLSeg22 s t cs) = f s <> f t <> foldMap (foldMap f) cs
instance Traversable (TLSeg22 'CheckEqual) where
    traverse f (TLSeg22 s t cs) =
        TLSeg22 <$> f s <*> f t <*> traverse (traverse f) cs

jcTLSeg22 :: Aeson.Options
jcTLSeg22 =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("tlSeg22" :: String)

instance (ToJSON   (CheckRep c a), ToJSON   a) => ToJSON   (TLSeg22 c a) where
    toJSON     = genericToJSON     jcTLSeg22
    toEncoding = genericToEncoding jcTLSeg22
instance (FromJSON (CheckRep c a), FromJSON a) => FromJSON (TLSeg22 c a) where
    parseJSON  = genericParseJSON  jcTLSeg22

-- | Silly pretty config to get my preferred layout easily.
--
-- Looks silly, but gets the job done very smoothly. Snoyman's yaml library is
-- based for exposing this.
ycTLSeg :: Yaml.Pretty.Config
ycTLSeg = Yaml.Pretty.setConfDropNull True $ Yaml.Pretty.setConfCompare f $ Yaml.Pretty.defConfig
  where
    f "type" _ = LT
    f _ "type" = GT
    f "source" _ = LT
    f _ "source" = GT
    f "translation" _ = LT
    f _ "translation" = GT
    f "meta" _ = LT
    f _ "meta" = GT
    f s1 s2 = compare s1 s2

genTL :: Env -> SCP' -> [TLSeg 'CheckEqual Text]
genTL env = concatMap go
  where
    go = \case
      -- Segments that contain text to translate. Some generation functions also
      -- handle the commentary, some are plain combinators.
      Seg05 tb  -> genTLTextbox env tb
      Seg09Choice csi (AW32Pairs cs) -> genTLChoiceOuter env csi (map fst cs)
      Seg22 s (AW32Pairs cs) -> [TLSeg22Choice' $ genTL22Choices env s (map fst cs)]
      Seg35 c -> [TLSeg35Choice' $ genTLChoice env c]

      -- Extra segments that are useful to know the presence of.
      Seg0B csi ci ->
        [ meta [ "Choice jump below. Check which choice & choice selection this corresponds to." ]
               [ ("choice_selection_index", tshow csi)
               , ("choice_index", tshow ci) ] ]
      Seg07SCP scp ->
        [ meta [ "Script jump. Any following text is likely accessed by a choice." ]
               [ ("scp_jump_target", scp) ] ]
      Seg0CFlag{} ->
        [ meta [ "0C command here. Alters flow (perhaps checks a flag)." ]
               [] ]

      -- Don't care about the rest.
      _ -> []

genTLTextbox :: Env -> Seg05Text 'Weak Text -> [TLSeg 'CheckEqual Text]
genTLTextbox env tb =
  [ TLSegComment' ( TLSegComment
    { scpTLCommentCommentary = []
    , scpTLCommentMeta       =
        let speakerName = fromMaybe "N/A" $ envSpeakerIDMap env $ seg05TextSpeakerID tb
         in Map.singleton "speaker" speakerName } )
  , TLSegTextbox'
    ( TLSegTextbox
      { tlSegTextboxSource      = seg05TextText tb
      , tlSegTextboxTranslation = envPendingPlaceholder env
      , tlSegTextboxOverflow    = Nothing } )
  ]

genTLChoiceOuter :: Env -> Natural -> [Text] -> [TLSeg 'CheckEqual Text]
genTLChoiceOuter env csi cs =
  [ meta [ "Choice selection below. Script flow jumps depending on selection." ]
         [ ("choice_selection_index", tshow csi) ]
  , TLSegChoice' $ map (genTLChoice env) cs ]

genTLChoice :: Env -> Text -> TLSegChoice 'CheckEqual Text
genTLChoice env c = TLSegChoice
                      { tlSegChoiceTranslation = envPendingPlaceholder env
                      , tlSegChoiceSource      = c }

genTL22Choices :: Env -> Text -> [Text] -> TLSeg22 'CheckEqual Text
genTL22Choices env s ss = TLSeg22
  { tlSeg22TopicSource = s
  , tlSeg22TopicTranslation = envPendingPlaceholder env
  , tlSeg22Choices = map (genTLChoice env) ss }

meta :: [Text] -> [(Text, Text)] -> TLSeg c s
meta cms kvs = TLSegComment' $ TLSegComment { scpTLCommentCommentary = cms
                                        , scpTLCommentMeta = Map.fromList kvs }

data Error
  = ErrorTLSegOverlong
  | ErrorSourceMismatch
  | ErrorTLSegTooShort
  | ErrorTypeMismatch
  | ErrorUnimplemented
    deriving (Generic, Eq, Show)

apply :: SCP' -> [TLSeg 'CheckEqual Text] -> Either Error SCP'
apply scp scptl =
    let (scpSegsTled, scptl') = runState (traverseM applySeg scp) scptl
     in case scpSegsTled of
          Left err -> Left err
          Right scpSegsTled' ->
            let scptl'' = skipToNextTL scptl'
             in case scptl'' of
                  _:_ -> Left ErrorTLSegOverlong
                  [] -> Right $ concat scpSegsTled'

skipToNextTL :: [TLSeg c a] -> [TLSeg c a]
skipToNextTL = \case []   -> []
                     a:as -> case a of
                               TLSegComment'{} -> skipToNextTL as
                               _ -> a:as

-- Using highly explicit/manual prisms here. Could clean up.
applySeg
    :: MonadState [TLSeg 'CheckEqual Text] m
    => Seg' -> m (Either Error [Seg'])
applySeg = \case
  Seg05 tb -> tryApplySeg tryExtractTextbox (tryApplySegTextbox tb)
  Seg09Choice n (AW32Pairs cs) -> tryApplySeg tryExtractChoice (tryApplySegChoice n cs)
  Seg22 topic (AW32Pairs cs) -> tryApplySeg tryExtract22 (tryApplySeg22 topic cs)
  Seg35 a -> tryApplySeg tryExtract35 (tryApplySeg35 a)
  seg -> return $ Right [seg]

tryExtractTextbox :: TLSeg c a -> Maybe (TLSegTextbox c a)
tryExtractTextbox = \case TLSegTextbox' a -> Just a
                          _               -> Nothing

tryExtractChoice :: TLSeg c a -> Maybe [TLSegChoice c a]
tryExtractChoice = \case TLSegChoice' a -> Just a
                         _              -> Nothing

tryExtract22 :: TLSeg c a -> Maybe (TLSeg22 c a)
tryExtract22 = \case TLSeg22Choice' a -> Just a
                     _                -> Nothing

tryExtract35 :: TLSeg c a -> Maybe (TLSegChoice c a)
tryExtract35 = \case TLSeg35Choice' a -> Just a
                     _                -> Nothing

tryApplySeg
    :: MonadState [TLSeg 'CheckEqual Text] m
    => (TLSeg 'CheckEqual Text -> Maybe a)
    -> (a -> Either Error [Seg'])
    -> m (Either Error [Seg'])
tryApplySeg f1 f2 = do
    (skipToNextTL <$> get) >>= \case
      []     -> return $ Left ErrorTLSegTooShort
      tl:tls -> do
        put tls
        case f1 tl of
          Nothing -> return $ Left ErrorTypeMismatch
          Just a  -> return $ f2 a

tryApplySegTextbox
    :: Seg05Text 'Weak Text -> TLSegTextbox 'CheckEqual Text
    -> Either Error [Seg']
tryApplySegTextbox tb tbTL
  | seg05TextText tb /= tlSegTextboxSource tbTL = Left ErrorSourceMismatch
  | otherwise = Right $ Seg05 tb' : overflow
  where
    tb' = tb { seg05TextText = tlSegTextboxTranslation tbTL }
    overflow = maybe [] fakeTextboxSeg $ tlSegTextboxOverflow tbTL
    fakeTextboxSeg text =
        [Seg05 $ tb { seg05TextVoiceLine = Text.empty
                        , seg05TextText      = text } ]

tryApplySegChoice
    :: Natural -> [(Text, Natural)] -> [TLSegChoice 'CheckEqual Text]
    -> Either Error [Seg']
tryApplySegChoice n cs csTL
  | length cs /= length csTL = Left ErrorSourceMismatch
  -- lol whatever XD
  | not (and (map (uncurry (==)) checks)) = Left ErrorSourceMismatch
  | otherwise = Right [Seg09Choice n (AW32Pairs edited)]
  where
    checks = zip (map tlSegChoiceSource csTL) (map fst cs)
    edited = zip (map tlSegChoiceTranslation csTL) (map snd cs)

tryApplySeg22
    :: Text -> [(Text, Natural)] -> (TLSeg22 'CheckEqual Text)
    -> Either Error [Seg']
tryApplySeg22 topic cs segTL
  | topic /= tlSeg22TopicSource segTL = Left ErrorSourceMismatch
  | length cs /= length csTL = Left ErrorSourceMismatch
  -- lol whatever XD
  | not (and (map (uncurry (==)) checks)) = Left ErrorSourceMismatch
  | otherwise = Right [Seg22 (tlSeg22TopicTranslation segTL) (AW32Pairs edited)]
  where
    csTL = tlSeg22Choices segTL
    checks = zip (map tlSegChoiceSource csTL) (map fst cs)
    edited = zip (map tlSegChoiceTranslation csTL) (map snd cs)

tryApplySeg35
    :: Text -> TLSegChoice 'CheckEqual Text
    -> Either Error [Seg']
tryApplySeg35 a aTL
  | a /= tlSegChoiceSource aTL = Left ErrorSourceMismatch
  | otherwise = Right [Seg35 (tlSegChoiceTranslation aTL)]

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f v'))
    -> t v
    -> m (f (t v'))
traverseM f xs = sequenceA <$> traverse f xs
