module Main where

import           GHC.Generics
import           Control.Monad.IO.Class
import           Options.Applicative
import qualified Tool.SCP.Replace
import qualified Tool.SCP.Code

main :: IO ()
main = execParserWithDefaults desc pCmd >>= \case
  CmdSCP scpCmd ->
    case scpCmd of
      CmdSCPReplace cfg -> Tool.SCP.Replace.run cfg
      CmdSCPEncode  cfg -> Tool.SCP.Code.runEncode cfg
      CmdSCPDecode  cfg -> Tool.SCP.Code.runDecode cfg
  where desc = "Various tools for working with GTVM assets."

data Cmd
  = CmdSCP CmdSCP
    deriving (Eq, Show, Generic)

data CmdSCP
  = CmdSCPEncode  Tool.SCP.Code.CfgEncode
  | CmdSCPDecode  Tool.SCP.Code.CfgDecode
  | CmdSCPReplace Tool.SCP.Replace.Cfg
    deriving (Eq, Show, Generic)

pCmd :: Parser Cmd
pCmd = hsubparser $
       cmd "scp" descSCP (CmdSCP <$> pCmdSCP)
  where
    descSCP = "Game script file (SCP, script/*.scp) tools."

pCmdSCP :: Parser CmdSCP
pCmdSCP = hsubparser $
       cmd "encode"  descEncode  (CmdSCPEncode  <$> Tool.SCP.Code.parseCLIOptsEncode)
    <> cmd "decode"  descDecode  (CmdSCPDecode  <$> Tool.SCP.Code.parseCLIOptsDecode)
    <> cmd "replace" descReplace (CmdSCPReplace <$> Tool.SCP.Replace.parseCLIOpts)
  where
    descEncode  = "Encode YAML SCP to binary."
    descDecode  = "Decode binary SCP to YAML."
    descReplace = "Replace textboxes (e.g. for translation)."

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

-- | Shorthand for the way I always write commands.
cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd name desc p = command name (info p (progDesc desc))
