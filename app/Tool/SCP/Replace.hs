module Tool.SCP.Replace where

import           GTVM.SCP.TextReplace
import           GTVM.SCP
import           Raehik.Check
import qualified CLI.Command            as CLI
import           Config
import           Util
import           Control.Monad.IO.Class
import           GHC.Generics
import           Data.Text              ( Text )

data Cfg = Cfg
  { cfgReplaceFile :: FilePath
  , cfgStreams     :: CStreamPair
  } deriving (Eq, Show, Generic)

cmd :: CLI.Cmd Cfg
cmd = CLI.Cmd "scpreplace" desc $
        Cfg <$> CLI.pFileIn "REPLACE_FILE" "replace (translation) file"
            <*> CLI.pCStreamPair "SCP" "SCP"
  where desc = "Replace textboxes in an SCP (e.g. for translation)."

run :: MonadIO m => Cfg -> m ()
run cfg = do
    trsBs <- readFileBytes $ cfgReplaceFile cfg
    trs   <- forceParseYaml @[SCPTextReplace 'CheckEqual Text] trsBs
    scpBs <- readStreamBytes $ cStreamPairIn $ cfgStreams cfg
    scp   <- forceParseYaml @(SCP Text) scpBs
    case replaceEqs trs scp of
      Left  err  -> naughtyExit "running a text replace" err
      Right scp' -> do
        let scp'Bs = encodeYamlPretty scp'
         in writeStreamTextualBytes (cStreamPairOut $ cfgStreams cfg) scp'Bs
