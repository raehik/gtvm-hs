module Tool.Flowchart where

import Common.Config
import Common.CLIOptions
import Common.Util
import Options.Applicative
import GHC.Generics
import Control.Monad.IO.Class
import Data.Text ( Text )
import GTVM.Flowchart
import Raehik.Binary
import Refined
import Refined.WithRefine
import Data.Serialize
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as Text

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
    fcText <- badParseYAML @(Flowchart 'Unenforced Text) fcBsYaml
    case enforceFlowchart (fcByteify fcText) of
      Left  err   -> liftIO $ print err
      Right fcBin -> do
        let fcBsBin = encode fcBin
        writeStreamBin (cfgEncodePrintBin cfg) (cfgEncodeStreamOut cfg) fcBsBin

runDecode :: MonadIO m => CfgDecode -> m ()
runDecode cfg = do
    fcBsBin <- readStreamBytes $ cfgDecodeStreamIn cfg
    case runGet (get @(Flowchart 'Enforced CString)) fcBsBin of
      Left  err   -> liftIO $ putStrLn err
      Right fcBin ->
        case fcTextify $ unenforceFlowchart fcBin of
          Left err -> liftIO $ print err
          Right fcText ->
            let fcBsYaml = encodeYamlPretty fcText
             in writeStreamTextualBytes (cfgDecodeStreamOut cfg) fcBsYaml
