module Tool.SCP.Code where

import Common.Config
import Common.CLIOptions
import Common.Util
import Options.Applicative
import GHC.Generics
import Control.Monad.IO.Class
import GTVM.SCP
import Binrep
import Binrep.Type.Text ( encodeToRep, decode )
import GTVM.Common.IO ( badParseYAML )
import Data.Yaml.Pretty qualified as Yaml.Pretty
import Data.ByteString qualified as B
import Refined

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
    scpYAML <- badParseYAML @SCPText scpYAMLBs
    scpBin :: SCPBin <- liftErr show $ do
        scpBin <- traverseSCP encodeToRep scpYAML
        refine' scpBin
    let scpBinBs = runPut scpBin
    writeStreamBin (cfgEncodePrintBin cfg) (cfgEncodeStreamOut cfg) scpBinBs

runDecode :: (MonadFail m, MonadIO m) => CfgDecode -> m ()
runDecode cfg = do
    scpBinBs <- readStreamBytes $ cfgDecodeStreamIn cfg
    (scpBin, bs) <- liftErr show $ runGet @SCPBin scpBinBs
    if not (B.null bs) then fail "dangling bytes"
    else do
        scpText :: SCPText <- liftErr id $ traverse (traverse (traverse decode)) $ scpWeaken scpBin
        let scpYAMLBs = Yaml.Pretty.encodePretty prettyYamlCfg scpText
        writeStreamTextualBytes (cfgDecodeStreamOut cfg) scpYAMLBs
