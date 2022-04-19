{-# LANGUAGE OverloadedStrings #-}

-- | Intent: A sum type with a constructor corresponding to each SCP macro that
--   stores user-facing text, with fields to allow checking & replacing such
--   text.
--
-- We need sum types if we want to handle everything in one place. CSVs don't
-- support sum types. YAML does. And with sum types, we can also generate
-- comments from a source SCP -- to e.g. say when another SCP is loaded.

module GTVM.SCP.TL where

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
import Data.Word
import Util ( tshow )

import Data.Yaml.Pretty qualified

import GTVM.SCP

import Control.Monad.State

data Env = Env
  { envPendingPlaceholder :: Text

  , envSpeakerIDMap       :: Maybe (Word32 -> Maybe Text)
  -- ^ Attempt to obtain a pretty speaker name from an ID.
  --
  -- This data isn't stored in the repo, and must instead be parsed at runtime.

  } deriving (Generic)

-- | See 'GTVM.SCP' for details.
data SCPTL (c :: Check) bs
  = SCPTLTextbox'  (SCPTLTextbox c bs)
  | SCPTLChoice'   [SCPTLChoice c bs]
  | SCPTL22Choice' (SCPTL22 c bs)
  | SCPTL35Choice' (SCPTLChoice c bs)
  | SCPTLComment'  SCPTLComment
    deriving (Generic)

deriving instance (Eq   (CheckRep c bs), Eq   bs) => Eq   (SCPTL c bs)
deriving instance (Show (CheckRep c bs), Show bs) => Show (SCPTL c bs)

deriving instance Functor     (SCPTL 'CheckEqual)
deriving instance Foldable    (SCPTL 'CheckEqual)
deriving instance Traversable (SCPTL 'CheckEqual)

jcSCPTL :: Aeson.Options
jcSCPTL = Aeson.defaultOptions
  { Aeson.constructorTagModifier = map Data.Char.toLower . init . drop 5
  , Aeson.sumEncoding = Aeson.TaggedObject
    { Aeson.tagFieldName = "type"
    , Aeson.contentsFieldName = "contents" }}

instance (ToJSON   (CheckRep c bs), ToJSON   bs) => ToJSON   (SCPTL c bs) where
    toJSON     = genericToJSON     jcSCPTL
    toEncoding = genericToEncoding jcSCPTL
instance (FromJSON (CheckRep c bs), FromJSON bs) => FromJSON (SCPTL c bs) where
    parseJSON  = genericParseJSON  jcSCPTL

data SCPTLComment = SCPTLComment
  { scpTLCommentCommentary :: [Text]
  , scpTLCommentMeta       :: Map Text Text
  } deriving stock (Generic, Eq, Show)

jcSCPTLComment :: Aeson.Options
jcSCPTLComment =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpTLComment" :: String)

instance ToJSON   SCPTLComment where
    toJSON     = genericToJSON     jcSCPTLComment
    toEncoding = genericToEncoding jcSCPTLComment
instance FromJSON SCPTLComment where
    parseJSON  = genericParseJSON  jcSCPTLComment

data SCPTLTextbox (c :: Check) bs = SCPTLTextbox
  { scpTLTextboxSource      :: CheckRep c bs
  , scpTLTextboxTranslation :: bs
  , scpTLTextboxOverflow    :: Maybe bs
  } deriving (Generic)

deriving instance (Eq   (CheckRep c bs), Eq   bs) => Eq   (SCPTLTextbox c bs)
deriving instance (Show (CheckRep c bs), Show bs) => Show (SCPTLTextbox c bs)

instance Functor     (SCPTLTextbox 'CheckEqual) where
    fmap     f (SCPTLTextbox s t o) = SCPTLTextbox (f s) (f t) (fmap f o)
instance Foldable    (SCPTLTextbox 'CheckEqual) where
    foldMap  f (SCPTLTextbox s t o) = f s <> f t <> foldMap f o
instance Traversable (SCPTLTextbox 'CheckEqual) where
    traverse f (SCPTLTextbox s t o) =
        SCPTLTextbox <$> f s <*> f t <*> traverse f o

jcSCPTLTextbox :: Aeson.Options
jcSCPTLTextbox =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpTLTextbox" :: String)

instance (ToJSON   (CheckRep c bs), ToJSON   bs) => ToJSON   (SCPTLTextbox c bs) where
    toJSON     = genericToJSON     jcSCPTLTextbox
    toEncoding = genericToEncoding jcSCPTLTextbox
instance (FromJSON (CheckRep c bs), FromJSON bs) => FromJSON (SCPTLTextbox c bs) where
    parseJSON  = genericParseJSON  jcSCPTLTextbox

data SCPTLChoice (c :: Check) bs = SCPTLChoice
  { scpTLChoiceSource      :: CheckRep c bs
  , scpTLChoiceTranslation :: bs
  } deriving (Generic)

deriving instance (Eq   (CheckRep c bs), Eq   bs) => Eq   (SCPTLChoice c bs)
deriving instance (Show (CheckRep c bs), Show bs) => Show (SCPTLChoice c bs)

instance Functor     (SCPTLChoice 'CheckEqual) where
    fmap     f (SCPTLChoice s t) = SCPTLChoice (f s) (f t)
instance Foldable    (SCPTLChoice 'CheckEqual) where
    foldMap  f (SCPTLChoice s t) = f s <> f t
instance Traversable (SCPTLChoice 'CheckEqual) where
    traverse f (SCPTLChoice s t) =
        SCPTLChoice <$> f s <*> f t

jcSCPTLChoice :: Aeson.Options
jcSCPTLChoice =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpTLChoice" :: String)

instance (ToJSON   (CheckRep c bs), ToJSON   bs) => ToJSON   (SCPTLChoice c bs) where
    toJSON     = genericToJSON     jcSCPTLChoice
    toEncoding = genericToEncoding jcSCPTLChoice
instance (FromJSON (CheckRep c bs), FromJSON bs) => FromJSON (SCPTLChoice c bs) where
    parseJSON  = genericParseJSON  jcSCPTLChoice

data SCPTL22 (c :: Check) bs = SCPTL22
  { scpTL22TopicSource      :: CheckRep c bs
  , scpTL22TopicTranslation :: bs
  , scpTL22Choices          :: [SCPTLChoice c bs]
  } deriving (Generic)

deriving instance (Eq   (CheckRep c bs), Eq   bs) => Eq   (SCPTL22 c bs)
deriving instance (Show (CheckRep c bs), Show bs) => Show (SCPTL22 c bs)

instance Functor     (SCPTL22 'CheckEqual) where
    fmap     f (SCPTL22 s t cs) = SCPTL22 (f s) (f t) (fmap (fmap f) cs)
instance Foldable    (SCPTL22 'CheckEqual) where
    foldMap  f (SCPTL22 s t cs) = f s <> f t <> foldMap (foldMap f) cs
instance Traversable (SCPTL22 'CheckEqual) where
    traverse f (SCPTL22 s t cs) =
        SCPTL22 <$> f s <*> f t <*> traverse (traverse f) cs

jcSCPTL22 :: Aeson.Options
jcSCPTL22 =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpTL22" :: String)

instance (ToJSON   (CheckRep c bs), ToJSON   bs) => ToJSON   (SCPTL22 c bs) where
    toJSON     = genericToJSON     jcSCPTL22
    toEncoding = genericToEncoding jcSCPTL22
instance (FromJSON (CheckRep c bs), FromJSON bs) => FromJSON (SCPTL22 c bs) where
    parseJSON  = genericParseJSON  jcSCPTL22

-- | Silly pretty config to get my preferred layout easily.
--
-- Looks silly, but gets the job done very smoothly. Snoyman's yaml library is
-- based for exposing this.
ycSCPTL :: Data.Yaml.Pretty.Config
ycSCPTL = Data.Yaml.Pretty.setConfDropNull True $ Data.Yaml.Pretty.setConfCompare f $ Data.Yaml.Pretty.defConfig
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

genTL :: Env -> SCP Text -> [SCPTL 'CheckEqual Text]
genTL env = concatMap go
  where
    go = \case
      -- Segments that contain text to translate. Some generation functions also
      -- handle the commentary, some are plain combinators.
      SCPSeg05Textbox tb  -> genTLTextbox env tb
      SCPSeg09Choice csi cs -> genTLChoiceOuter env csi (map fst cs)
      SCPSeg22 s cs -> [SCPTL22Choice' $ genTL22Choices env s (map fst cs)]
      SCPSeg35 c -> [SCPTL35Choice' $ genTLChoice env c]

      -- Extra segments that are useful to know the presence of.
      SCPSeg0B csi ci ->
        [ meta [ "Choice jump below. Check which choice & choice selection this corresponds to." ]
               [ ("choice_selection_index", tshow csi)
               , ("choice_index", tshow ci) ] ]
      SCPSeg07SCP scp ->
        [ meta [ "Script jump. Any following text is likely accessed by a choice." ]
               [ ("scp_jump_target", scp) ] ]
      SCPSeg0CFlag{} ->
        [ meta [ "0C command here. Alters flow (perhaps checks a flag)." ]
               [] ]

      -- Don't care about the rest.
      _ -> []

genTLTextbox :: Env -> SCPSeg05Textbox Text -> [SCPTL 'CheckEqual Text]
genTLTextbox env tb =
  [ SCPTLComment' ( SCPTLComment
    { scpTLCommentCommentary = []
    , scpTLCommentMeta       =
        case envSpeakerIDMap env of
          Nothing         -> Map.empty
          Just speakerMap ->
            let speakerName = fromMaybe "N/A" $ speakerMap $ scpSeg05TextboxSpeakerID tb
             in Map.singleton "speaker" speakerName } )
  , SCPTLTextbox'
    ( SCPTLTextbox
      { scpTLTextboxSource      = scpSeg05TextboxText tb
      , scpTLTextboxTranslation = envPendingPlaceholder env
      , scpTLTextboxOverflow    = Nothing } )
  ]

genTLChoiceOuter :: Env -> Word8 -> [Text] -> [SCPTL 'CheckEqual Text]
genTLChoiceOuter env csi cs =
  [ meta [ "Choice selection below. Script flow jumps depending on selection." ]
         [ ("choice_selection_index", tshow csi) ]
  , SCPTLChoice' $ map (genTLChoice env) cs ]

genTLChoice :: Env -> Text -> SCPTLChoice 'CheckEqual Text
genTLChoice env c = SCPTLChoice
                      { scpTLChoiceTranslation = envPendingPlaceholder env
                      , scpTLChoiceSource      = c }

genTL22Choices :: Env -> Text -> [Text] -> SCPTL22 'CheckEqual Text
genTL22Choices env s ss = SCPTL22
  { scpTL22TopicSource = s
  , scpTL22TopicTranslation = envPendingPlaceholder env
  , scpTL22Choices = map (genTLChoice env) ss }

meta :: [Text] -> [(Text, Text)] -> SCPTL c s
meta cms kvs = SCPTLComment' $ SCPTLComment { scpTLCommentCommentary = cms
                                        , scpTLCommentMeta = Map.fromList kvs }

data Error
  = ErrorSCPTLOverlong
  | ErrorSourceMismatch
  | ErrorSCPTLTooShort
  | ErrorTypeMismatch
  | ErrorUnimplemented
    deriving (Generic, Eq, Show)

apply :: SCP Text -> [SCPTL 'CheckEqual Text] -> Either Error (SCP Text)
apply scp scptl =
    let (scpSegsTled, scptl') = runState (traverseM applySeg scp) scptl
     in case scpSegsTled of
          Left err -> Left err
          Right scpSegsTled' ->
            let scptl'' = skipToNextTL scptl'
             in case scptl'' of
                  _:_ -> Left ErrorSCPTLOverlong
                  [] -> Right $ concat scpSegsTled'

skipToNextTL :: [SCPTL c bs] -> [SCPTL c bs]
skipToNextTL = \case []   -> []
                     a:as -> case a of
                               SCPTLComment'{} -> skipToNextTL as
                               _ -> a:as

-- Using highly explicit/manual prisms here. Could clean up.
applySeg
    :: MonadState [SCPTL 'CheckEqual Text] m
    => SCPSeg Text -> m (Either Error [SCPSeg Text])
applySeg = \case
  SCPSeg05Textbox tb -> tryApplySeg tryExtractTextbox (tryApplySegTextbox tb)
  SCPSeg09Choice w8 cs -> tryApplySeg tryExtractChoice (tryApplySegChoice w8 cs)
  SCPSeg22 topic cs -> tryApplySeg tryExtract22 (tryApplySeg22 topic cs)
  SCPSeg35 bs -> tryApplySeg tryExtract35 (tryApplySeg35 bs)
  seg -> return $ Right [seg]

tryExtractTextbox :: SCPTL c bs -> Maybe (SCPTLTextbox c bs)
tryExtractTextbox = \case SCPTLTextbox' a -> Just a
                          _               -> Nothing

tryExtractChoice :: SCPTL c bs -> Maybe [SCPTLChoice c bs]
tryExtractChoice = \case SCPTLChoice' a -> Just a
                         _              -> Nothing

tryExtract22 :: SCPTL c bs -> Maybe (SCPTL22 c bs)
tryExtract22 = \case SCPTL22Choice' a -> Just a
                     _                -> Nothing

tryExtract35 :: SCPTL c bs -> Maybe (SCPTLChoice c bs)
tryExtract35 = \case SCPTL35Choice' a -> Just a
                     _                -> Nothing

tryApplySeg
    :: MonadState [SCPTL 'CheckEqual Text] m
    => (SCPTL 'CheckEqual Text -> Maybe a)
    -> (a -> Either Error [SCPSeg Text])
    -> m (Either Error [SCPSeg Text])
tryApplySeg f1 f2 = do
    (skipToNextTL <$> get) >>= \case
      []     -> return $ Left ErrorSCPTLTooShort
      tl:tls -> do
        put tls
        case f1 tl of
          Nothing -> return $ Left ErrorTypeMismatch
          Just a  -> return $ f2 a

tryApplySegTextbox
    :: SCPSeg05Textbox Text -> SCPTLTextbox 'CheckEqual Text
    -> Either Error [SCPSeg Text]
tryApplySegTextbox tb tbTL
  | scpSeg05TextboxText tb /= scpTLTextboxSource tbTL = Left ErrorSourceMismatch
  | otherwise = Right $ SCPSeg05Textbox tb' : overflow
  where
    tb' = tb { scpSeg05TextboxText = scpTLTextboxTranslation tbTL }
    overflow = maybe [] fakeTextboxSeg $ scpTLTextboxOverflow tbTL
    fakeTextboxSeg text =
        [SCPSeg05Textbox $ tb { scpSeg05TextboxVoiceLine = Text.empty
                              , scpSeg05TextboxText      = text } ]

tryApplySegChoice
    :: Word8 -> [(Text, Word32)] -> [SCPTLChoice 'CheckEqual Text]
    -> Either Error [SCPSeg Text]
tryApplySegChoice w8 cs csTL
  | length cs /= length csTL = Left ErrorSourceMismatch
  -- lol whatever XD
  | not (and (map (uncurry (==)) checks)) = Left ErrorSourceMismatch
  | otherwise = Right [SCPSeg09Choice w8 edited]
  where
    checks = zip (map scpTLChoiceSource csTL) (map fst cs)
    edited = zip (map scpTLChoiceTranslation csTL) (map snd cs)

tryApplySeg22
    :: Text -> [(Text, Word32)] -> (SCPTL22 'CheckEqual Text)
    -> Either Error [SCPSeg Text]
tryApplySeg22 topic cs segTL
  | topic /= scpTL22TopicSource segTL = Left ErrorSourceMismatch
  | length cs /= length csTL = Left ErrorSourceMismatch
  -- lol whatever XD
  | not (and (map (uncurry (==)) checks)) = Left ErrorSourceMismatch
  | otherwise = Right [SCPSeg22 (scpTL22TopicTranslation segTL) edited]
  where
    csTL = scpTL22Choices segTL
    checks = zip (map scpTLChoiceSource csTL) (map fst cs)
    edited = zip (map scpTLChoiceTranslation csTL) (map snd cs)

tryApplySeg35
    :: Text -> SCPTLChoice 'CheckEqual Text
    -> Either Error [SCPSeg Text]
tryApplySeg35 bs bsTL
  | bs /= scpTLChoiceSource bsTL = Left ErrorSourceMismatch
  | otherwise = Right [SCPSeg35 (scpTLChoiceTranslation bsTL)]

-- lol. ty hw-kafka-client
traverseM
    :: (Traversable t, Applicative f, Monad m)
    => (v -> m (f v'))
    -> t v
    -> m (f (t v'))
traverseM f xs = sequenceA <$> traverse f xs
