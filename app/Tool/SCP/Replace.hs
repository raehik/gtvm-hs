module Tool.SCP.Replace where

import           GHC.Generics
import           Control.Monad.IO.Class
import           Common.Config
import           Common.CLIOptions
import           Options.Applicative
import           Data.Text              ( Text )
import           GTVM.SCP
import           GTVM.SCP.TextReplace
import           Raehik.Check
import           Common.Util

data Cfg = Cfg
  { cfgReplaceFile :: StreamFile 'StreamIn  "replace data"
  , cfgIn          :: Stream     'StreamIn  "YAML SCP"
  , cfgOut         :: Stream     'StreamOut "YAML SCP"
  } deriving (Eq, Show, Generic)

parseCLIOpts :: Parser Cfg
parseCLIOpts = Cfg <$> pStreamFileIn <*> pStreamIn <*> pStreamOut

run :: MonadIO m => Cfg -> m ()
run cfg = do
    trsBs <- readStreamFileBytes $ cfgReplaceFile cfg
    trs   <- badParseYAML @[SCPTextReplace 'CheckEqual Text] trsBs
    scpBs <- readStreamBytes $ cfgIn cfg
    scp   <- badParseYAML @(SCP Text) scpBs
    case replaceEqs trs scp of
      Left  err  -> badExit "running a text replace" err
      Right scp' -> do
        let scp'Bs = encodeYamlPretty scp'
         in writeStreamTextualBytes (cfgOut cfg) scp'Bs
