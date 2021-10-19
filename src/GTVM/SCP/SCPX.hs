module GTVM.SCP.SCPX where

import           GHC.Generics
import           Data.Word
import           Data.Aeson
import qualified Data.Text      as Text
import           Data.Text      ( Text )
import           Data.String    ( IsString(..) )
import           Data.Maybe     ( fromMaybe )

import           GTVM.SCP

data SCPX bs
  = SCPXPrimitive     (SCPSegment bs)
  | SCPXPrimitiveList [SCPSegment bs] -- ^ (speedup because this is common)
  | SCPXText SCPXText
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

jcSCPX :: Options
jcSCPX = defaultOptions
  { constructorTagModifier = camelTo2 '_' . drop 4
  , sumEncoding = defaultTaggedObject
    { tagFieldName = "command"
    , contentsFieldName = "arguments" }}

instance ToJSON   a => ToJSON   (SCPX a) where
    toJSON     = genericToJSON     jcSCPX
    toEncoding = genericToEncoding jcSCPX
instance FromJSON a => FromJSON (SCPX a) where
    parseJSON  = genericParseJSON  jcSCPX

data SCPXText = SCPXText'
  { scpXTextText      :: Text
  , scpXTextSpeaker   :: Speaker
  , scpXTextVoiceLine :: Maybe Text
  } deriving (Eq, Show, Generic)

jcSCPXText :: Options
jcSCPXText = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length "scpXText") }

instance ToJSON   SCPXText where
    toJSON     = genericToJSON     jcSCPXText
    toEncoding = genericToEncoding jcSCPXText
instance FromJSON SCPXText where
    parseJSON  = genericParseJSON  jcSCPXText

scpXTextTransform :: IsString bs => SCPXText -> SCPSeg05Textbox bs
scpXTextTransform x = SCPSeg05Textbox'
    { scpSeg05TextboxSpeakerUnkCharID = speakerUnkCharId
    , scpSeg05TextboxSpeakerID        = speakerId
    , scpSeg05TextboxText             = fromString $ Text.unpack $ scpXTextText x
    , scpSeg05TextboxVoiceLine        = fromString $ Text.unpack $ fromMaybe Text.empty $ scpXTextVoiceLine x
    , scpSeg05TextboxCounter          = 0 -- TODO
    }
  where
    (speakerUnkCharId, speakerId) = speakerInfo (scpXTextSpeaker x)

evalSCPX :: IsString bs => [SCPX bs] -> [SCPSegment bs]
evalSCPX []     = []
evalSCPX (x:xs) =
    case x of
      SCPXPrimitive     seg  -> seg : evalSCPX xs
      SCPXPrimitiveList segs -> foldr (:) (evalSCPX xs) segs
      SCPXText          scpx -> SCPSeg05Textbox (scpXTextTransform scpx) : evalSCPX xs

-- QM = question mark (at end)
-- Unk = unknown (written as full ???)
data Speaker
  = SpeakerBanri
  | SpeakerSao
  | SpeakerReikonBanriUnk
  | SpeakerReikonBanriQM
  | SpeakerReikonBanri
    deriving (Eq, Show, Generic)

speakerInfo :: Speaker -> (Word8, Word32)
speakerInfo = \case
  SpeakerBanri          -> (5, 5)
  SpeakerSao            -> (8, 8)
  SpeakerReikonBanriUnk -> (16, 17)
  SpeakerReikonBanriQM  -> (16, 18)
  SpeakerReikonBanri    -> (16, 16)

jcSpeaker :: Options
jcSpeaker = defaultOptions { constructorTagModifier = camelTo2 '_' . drop 7 }

instance ToJSON   Speaker where
    toJSON     = genericToJSON     jcSpeaker
    toEncoding = genericToEncoding jcSpeaker
instance FromJSON Speaker where
    parseJSON  = genericParseJSON  jcSpeaker
