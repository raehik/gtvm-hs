module GTVM.SCP.SCPX where

import           GHC.Generics
import           Data.Word
import           Data.Aeson
import           GTVM.Common.Json
import qualified Data.Text      as Text
import           Data.Text      ( Text )
import           Data.String    ( IsString(..) )
import           Data.Maybe     ( fromMaybe )

import           GTVM.SCP

type SCPX bs = [SCPXSeg bs]

data SCPXSeg bs
  = SCPXSegPrimitive     (SCPSeg bs)
  | SCPXSegPrimitiveList [SCPSeg bs] -- ^ (speedup because this is common)
  | SCPXSegText SCPXSegText
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

jcSCPXSeg :: Options
jcSCPXSeg = defaultOptions
  { constructorTagModifier = camelTo2 '_' . drop 4
  , sumEncoding = defaultTaggedObject
    { tagFieldName = "command"
    , contentsFieldName = "arguments" }}

instance ToJSON   a => ToJSON   (SCPXSeg a) where
    toJSON     = genericToJSON     jcSCPXSeg
    toEncoding = genericToEncoding jcSCPXSeg
instance FromJSON a => FromJSON (SCPXSeg a) where
    parseJSON  = genericParseJSON  jcSCPXSeg

data SCPXSegText = SCPXSegText'
  { scpXSegTextText      :: Text
  , scpXSegTextSpeaker   :: Speaker
  , scpXSegTextVoiceLine :: Maybe Text
  } deriving (Eq, Show, Generic)

jcSCPXText :: Options
jcSCPXText = jsonCfgSepUnderscoreDropN (fromIntegral (length "scpXText"))

instance ToJSON   SCPXSegText where
    toJSON     = genericToJSON     jcSCPXText
    toEncoding = genericToEncoding jcSCPXText
instance FromJSON SCPXSegText where
    parseJSON  = genericParseJSON  jcSCPXText

scpXSegTextTransform :: IsString bs => SCPXSegText -> SCPSeg05Textbox bs
scpXSegTextTransform x = SCPSeg05Textbox'
    { scpSeg05TextboxSpeakerUnkCharID = speakerUnkCharId
    , scpSeg05TextboxSpeakerID        = speakerId
    , scpSeg05TextboxText             = fromString $ Text.unpack $ scpXSegTextText x
    , scpSeg05TextboxVoiceLine        = fromString $ Text.unpack $ fromMaybe Text.empty $ scpXSegTextVoiceLine x
    , scpSeg05TextboxCounter          = 0 -- TODO
    }
  where
    (speakerUnkCharId, speakerId) = speakerInfo (scpXSegTextSpeaker x)

evalSCPX :: IsString bs => SCPX bs -> SCP bs
evalSCPX []     = []
evalSCPX (x:xs) =
    case x of
      SCPXSegPrimitive     seg  -> seg : evalSCPX xs
      SCPXSegPrimitiveList segs -> foldr (:) (evalSCPX xs) segs
      SCPXSegText          scpx -> SCPSeg05Textbox (scpXSegTextTransform scpx) : evalSCPX xs

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
