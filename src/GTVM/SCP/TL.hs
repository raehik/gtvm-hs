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
import Data.Map ( Map )
import Data.Map qualified as Map
import Data.Char qualified
import Data.Maybe ( fromMaybe )
import Data.Word
import Util ( tshow )

import Data.Yaml.Pretty qualified

import GTVM.SCP

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

jcSCPTL :: Aeson.Options
jcSCPTL = Aeson.defaultOptions
  { Aeson.constructorTagModifier = map Data.Char.toLower . init . drop 5
  , Aeson.sumEncoding = Aeson.defaultTaggedObject
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

meta :: [Text] -> [(Text, Text)] -> SCPTL _c _s
meta cms kvs = SCPTLComment' $ SCPTLComment { scpTLCommentCommentary = cms
                                        , scpTLCommentMeta = Map.fromList kvs }
