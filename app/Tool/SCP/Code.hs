module Tool.SCP.Code where

import           Common.Config
import           Common.CLIOptions
import           Common.Util
import           Options.Applicative
import           GHC.Generics
import           Control.Monad.IO.Class
import           Data.Text                  ( Text )
import           GTVM.SCP
import qualified GTVM.SCP.Serialize         as GSS
import qualified GTVM.SCP.Parse             as GSP
import qualified GTVM.Common.Binary         as GCB
import qualified GTVM.Common.Binary.Parse   as GCBP

data CfgEncode = CfgEncode
  { cfgEncodeStreamIn  :: Stream 'StreamIn  "YAML SCP"
  , cfgEncodeStreamOut :: Stream 'StreamOut "binary SCP"
  , cfgEncodePrintBin  :: PrintBin
  } deriving (Eq, Show, Generic)

data CfgDecode = CfgDecode
  { cfgDecodeStreamIn  :: Stream 'StreamIn  "binary SCP"
  , cfgDecodeStreamOut :: Stream 'StreamOut "YAML SCP"
  } deriving (Eq, Show, Generic)

parseCLIOptsEncode :: Parser CfgEncode
parseCLIOptsEncode = CfgEncode <$> pStreamIn <*> pStreamOut <*> pPrintBin

parseCLIOptsDecode :: Parser CfgDecode
parseCLIOptsDecode = CfgDecode <$> pStreamIn <*> pStreamOut

runEncode :: MonadIO m => CfgEncode -> m ()
runEncode cfg = do
    scpYAMLBs <- readStreamBytes $ cfgEncodeStreamIn cfg
    scp <- badParseYAML @(SCP Text) scpYAMLBs
    let scp'  = scpTextToBs scp
        scpBs = GSS.sSCP scp' GCB.binCfgSCP
    writeStreamBin (cfgEncodePrintBin cfg) (cfgEncodeStreamOut cfg) scpBs

runDecode :: MonadIO m => CfgDecode -> m ()
runDecode cfg = do
    scp <- badParseStream (GCBP.parseBin GSP.pSCP GCB.binCfgSCP) $ cfgDecodeStreamIn cfg
    case scpBsToText scp of
      Left err ->
        badExit "decoding UTF-8 bytestring" err
      Right scp' ->
        let scpYAMLBs = encodeYamlPretty scp'
         in writeStreamTextualBytes (cfgDecodeStreamOut cfg) scpYAMLBs
