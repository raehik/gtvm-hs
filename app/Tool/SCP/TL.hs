{-# LANGUAGE OverloadedStrings #-}

module Tool.SCP.TL where

import Common.Config
import Common.CLIOptions
import Common.Util
import Common.IO ( badParseYAML )

import Tool.SCP.Common ( scpPrettyYamlCfg )

import GTVM.Internal.Json

import Options.Applicative

import GHC.Generics
import Control.Monad.IO.Class

import GTVM.SCP.TL

import Data.Text ( Text )
import Data.Yaml.Pretty qualified
import Data.Map qualified as Map

import Data.Yaml.Pretty qualified as Yaml.Pretty

import Numeric.Natural ( Natural )

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
          Nothing -> return $ const Nothing
          Just sidmapfp -> parseSpeakerMap sidmapfp
    let env = Env
                { envPendingPlaceholder = "TODO not yet translated"
                , envSpeakerIDMap       = speakerIDMap }
    let scpIn = cfgToSCPTLStreamIn cfg
    scpYAMLBs <- readStreamBytes scpIn
    scp <- badParseYAML @SCP' scpYAMLBs
    let scptl   = genTL env scp
        scptlBs = Data.Yaml.Pretty.encodePretty ycTLSeg scptl
    writeStreamTextualBytes (cfgToSCPTLStreamOut cfg) scptlBs

-- | Silly pretty config to get my preferred layout easily.
--
-- Looks silly, but gets the job done very smoothly. Snoyman's yaml library is
-- based for exposing this.
ycTLSeg :: Yaml.Pretty.Config
ycTLSeg =
      Yaml.Pretty.setConfDropNull True
    $ Yaml.Pretty.setConfCompare tlSegFieldOrdering
    $ Yaml.Pretty.defConfig

data SCPSpeakerData = SCPSpeakerData
  { scpSpeakerDataStringAtPointer :: Text
  } deriving (Generic, Eq, Show)

-- TODO previously was rejectUnknownFields = False, now True...
instance ToJSON   SCPSpeakerData where
    toJSON     = gtjg "scpSpeakerData"
    toEncoding = gteg "scpSpeakerData"
instance FromJSON SCPSpeakerData where
    parseJSON  = gpjg "scpSpeakerData"

parseSpeakerMap :: MonadIO m => StreamFile 'StreamIn s -> m (Natural -> Maybe Text)
parseSpeakerMap fp = do
    bs <- readStreamFileBytes fp
    speakers <- badParseYAML @[SCPSpeakerData] bs
    let speakerMap = Map.fromList $ zip [1..] speakers
    return $ \n -> scpSpeakerDataStringAtPointer <$> Map.lookup n speakerMap

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
    scp       <- badParseYAML @SCP' scpYAMLBs
    scptlYAMLBs <- readStreamFileBytes $ cfgApplySCPTLFileIn cfg
    scptl       <- badParseYAML @SCPTL' scptlYAMLBs
    case apply scp scptl of
      Left  err  -> error $ show err
      Right scp' ->
        let scpYAMLBs' = Yaml.Pretty.encodePretty scpPrettyYamlCfg scp'
         in writeStreamTextualBytes (cfgApplySCPTLStreamOut cfg) scpYAMLBs'
