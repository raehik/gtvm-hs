-- | TODO deforestize plz...

module Main where

import GHC.Generics
import Control.Monad.IO.Class
import Options.Applicative
import Tool.SCP.Replace
import Tool.SCP.Code qualified
import Tool.SL01 qualified

main :: IO ()
main = execParserWithDefaults desc pCmd >>= \case
  CmdSCP scpCmd ->
    case scpCmd of
      CmdSCPReplace cfg -> Tool.SCP.Replace.run    cfg
      CmdSCPEncode  cfg -> Tool.SCP.Code.runEncode cfg
      CmdSCPDecode  cfg -> Tool.SCP.Code.runDecode cfg
  CmdSL01 sl01Cmd ->
    case sl01Cmd of
      CmdSL01Compress   cfg -> Tool.SL01.runCompress   cfg
      CmdSL01Decompress cfg -> Tool.SL01.runDecompress cfg
  where desc = "Various tools for working with GTVM assets."

data Cmd
  = CmdSCP  CmdSCP
  | CmdSL01 CmdSL01
    deriving (Eq, Show, Generic)

data CmdSCP
  = CmdSCPEncode  Tool.SCP.Code.CfgEncode
  | CmdSCPDecode  Tool.SCP.Code.CfgDecode
  | CmdSCPReplace Tool.SCP.Replace.Cfg
    deriving (Eq, Show, Generic)

data CmdSL01
  = CmdSL01Compress   Tool.SL01.CfgCompress
  | CmdSL01Decompress Tool.SL01.CfgDecompress
    deriving (Eq, Show, Generic)

pCmd :: Parser Cmd
pCmd = hsubparser $
       cmd "scp"  descSCP  (CmdSCP  <$> pCmdSCP)
    <> cmd "sl01" descSL01 (CmdSL01 <$> pCmdSL01)
  where
    descSCP  = "Game script file (SCP, script/*.scp) tools."
    descSL01 = "SL01 (LZO1x-compressed file) tools."

pCmdSCP :: Parser CmdSCP
pCmdSCP = hsubparser $
       cmd "encode"  descEncode  (CmdSCPEncode  <$> Tool.SCP.Code.parseCLIOptsEncode)
    <> cmd "decode"  descDecode  (CmdSCPDecode  <$> Tool.SCP.Code.parseCLIOptsDecode)
    <> cmd "replace" descReplace (CmdSCPReplace <$> Tool.SCP.Replace.parseCLIOpts)
  where
    descEncode  = "Encode YAML SCP to binary."
    descDecode  = "Decode binary SCP to YAML."
    descReplace = "Replace textboxes (e.g. for translation)."

pCmdSL01 :: Parser CmdSL01
pCmdSL01 = hsubparser $
       cmd "compress"   descCompress   (CmdSL01Compress   <$> Tool.SL01.parseCLIOptsCompress)
    <> cmd "decompress" descDecompress (CmdSL01Decompress <$> Tool.SL01.parseCLIOptsDecompress)
  where
    descCompress   = "Compress game data."
    descDecompress = "Decompress game data."

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

-- | Shorthand for the way I always write commands.
cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd name desc p = command name (info p (progDesc desc))
