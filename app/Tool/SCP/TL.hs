{-# LANGUAGE OverloadedStrings #-}

module Tool.SCP.TL where

import Common.Config
import Common.CLIOptions
import Common.Util
import Options.Applicative

import GHC.Generics
import Control.Monad.IO.Class

import GTVM.SCP
import GTVM.SCP.TL
import Raehik.Check

import Data.Text ( Text )
import Data.Yaml.Pretty qualified
import Data.Word
import Data.Map qualified as Map

import Data.Aeson
import Data.Aeson qualified as Aeson
import GTVM.Common.Json

data CfgToSCPTL = CfgToSCPTL
  { cfgToSCPTLStreamIn     :: Stream 'StreamIn  "YAML SCP"
  , cfgToSCPTLStreamOut    :: Stream 'StreamOut "SCPTL"
  , cfgToSCPTLSpeakerIDMap :: Maybe (StreamFile 'StreamIn "Speaker ID map YAML")
  } deriving (Eq, Show, Generic)

parseCLIOptsToSCPTL :: Parser CfgToSCPTL
parseCLIOptsToSCPTL =
    CfgToSCPTL <$> pStreamIn <*> pStreamOut <*> optional speakerIDMapOpt
  where
    speakerIDMapOpt =
        StreamFile <$> strOption (    long "speaker-ids"
                                   <> help "File containing speaker IDs" )

runToSCPTL :: MonadIO m => CfgToSCPTL -> m ()
runToSCPTL cfg = do
    speakerIDMap <- do
        case cfgToSCPTLSpeakerIDMap cfg of
          Nothing -> return Nothing
          Just sidmapfp -> Just <$> parseSpeakerMap sidmapfp
    let env = Env
                { envPendingPlaceholder = "TODO not yet translated"
                , envSpeakerIDMap       = speakerIDMap }
    let scpIn = cfgToSCPTLStreamIn cfg
    scpYAMLBs <- readStreamBytes scpIn
    scp <- badParseYAML @(SCP Text) scpYAMLBs
    let scptl   = genTL env scp
        scptlBs = Data.Yaml.Pretty.encodePretty ycSCPTL scptl
    writeStreamTextualBytes (cfgToSCPTLStreamOut cfg) scptlBs

data SCPSpeakerData = SCPSpeakerData
  { scpSpeakerDataStringAtPointer :: Text
  } deriving (Generic, Eq, Show)

jcSCPSpeakerData :: Aeson.Options
jcSCPSpeakerData =
    (jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpSpeakerData" :: String))
      { rejectUnknownFields = False }

instance ToJSON   SCPSpeakerData where
    toJSON     = genericToJSON     jcSCPSpeakerData
    toEncoding = genericToEncoding jcSCPSpeakerData
instance FromJSON SCPSpeakerData where
    parseJSON  = genericParseJSON  jcSCPSpeakerData

parseSpeakerMap :: MonadIO m => StreamFile 'StreamIn _s -> m (Word32 -> Maybe Text)
parseSpeakerMap fp = do
    bs <- readStreamFileBytes fp
    speakers <- badParseYAML @[SCPSpeakerData] bs
    let speakerMap = Map.fromList $ zip [1..] speakers
    return $ \w32 -> scpSpeakerDataStringAtPointer <$> Map.lookup w32 speakerMap

data CfgApplySCPTL = CfgApplySCPTL
  { cfgApplySCPTLStreamIn  :: Stream     'StreamIn  "YAML SCP"
  , cfgApplySCPTLFileIn    :: StreamFile 'StreamIn  "SCPTL"
  , cfgApplySCPTLStreamOut :: Stream     'StreamOut "edited YAML SCP"
  } deriving (Eq, Show, Generic)

parseCLIOptsApplySCPTL :: Parser CfgApplySCPTL
parseCLIOptsApplySCPTL =
    CfgApplySCPTL <$> pStreamIn <*> pStreamFileIn <*> pStreamOut

runApplySCPTL :: MonadIO m => CfgApplySCPTL -> m ()
runApplySCPTL cfg = do
    scpYAMLBs <- readStreamBytes $ cfgApplySCPTLStreamIn cfg
    scp       <- badParseYAML @(SCP Text) scpYAMLBs
    scptlYAMLBs <- readStreamFileBytes $ cfgApplySCPTLFileIn cfg
    scptl       <- badParseYAML @[SCPTL 'CheckEqual Text] scptlYAMLBs
    case apply scp scptl of
      Left  err  -> error $ show err
      Right scp' ->
        let scpYAMLBs' = encodeYamlPretty scp'
         in writeStreamTextualBytes (cfgApplySCPTLStreamOut cfg) scpYAMLBs'
