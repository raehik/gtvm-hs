{-# LANGUAGE OverloadedStrings #-}

module Tool.Flowchart where

import Common.Config
import Common.CLIOptions
import Common.Util
import GTVM.Flowchart

import Binrep
import Strongweak

import Options.Applicative
import GHC.Generics ( Generic )
import Control.Monad.IO.Class
import Data.Yaml.Pretty qualified
import GTVM.Common.IO ( badParseYAML )
import Data.ByteString qualified as BS

import Binrep.Type.ByteString
import Binrep.Type.Text

data CfgEncode = CfgEncode
  { cfgEncodeStreamIn  :: Stream 'StreamIn  "YAML flowchart"
  , cfgEncodeStreamOut :: Stream 'StreamOut "binary flowchart"
  , cfgEncodePrintBin  :: PrintBin
  } deriving (Eq, Show, Generic)

data CfgDecode = CfgDecode
  { cfgDecodeStreamIn  :: Stream 'StreamIn  "binary flowchart"
  , cfgDecodeStreamOut :: Stream 'StreamOut "YAML flowchart"
  } deriving (Eq, Show, Generic)

parseCLIOptsEncode :: Parser CfgEncode
parseCLIOptsEncode = CfgEncode <$> pStreamIn <*> pStreamOut <*> pPrintBin

parseCLIOptsDecode :: Parser CfgDecode
parseCLIOptsDecode = CfgDecode <$> pStreamIn <*> pStreamOut

runEncode :: MonadIO m => CfgEncode -> m ()
runEncode cfg = do
    fcBsYaml <- readStreamBytes $ cfgEncodeStreamIn cfg
    fcText   <- badParseYAML fcBsYaml
    fcBin    <- liftErr show $ fcBytesRefine $ fcTextToBytes @'UTF8 fcText
    fcBin'   <- liftValidation strengthenErrorPretty $ strengthen fcBin
    let fcBsBin = runPut @(Flowchart 'Strong (AsByteString 'C)) fcBin'
    writeStreamBin (cfgEncodePrintBin cfg) (cfgEncodeStreamOut cfg) fcBsBin

runDecode :: MonadIO m => CfgDecode -> m ()
runDecode cfg = do
    fcBsBin <- readStreamBytes $ cfgDecodeStreamIn cfg
    (fcBin, bs) <- liftErr id  $ runGet @(Flowchart 'Strong (AsByteString 'C)) fcBsBin
    case BS.null bs of
      False -> error "TODO bytes left over"
      True -> do
        fcText  <- liftErr show $ fcBytesToText @'UTF8 $ weaken fcBin
        let fcBsYaml = Data.Yaml.Pretty.encodePretty ycFCTL fcText
        writeStreamTextualBytes (cfgDecodeStreamOut cfg) fcBsYaml

-- | Silly pretty config to get my preferred layout easily.
--
-- Looks silly, but gets the job done very smoothly. Snoyman's yaml library is
-- based for exposing this.
ycFCTL :: Data.Yaml.Pretty.Config
ycFCTL = Data.Yaml.Pretty.setConfCompare f $ Data.Yaml.Pretty.defConfig
  where f "entries" _ = GT
        f _ "entries" = LT
        f "script" _ = LT
        f _ "script" = GT
        f "index" _ = GT
        f _ "index" = LT
        f s1 s2 = compare s1 s2
