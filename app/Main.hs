-- | TODO deforestize plz...

module Main where

import GHC.Generics
import Control.Monad.IO.Class
import Options.Applicative
import Tool.SCP.Replace
import Tool.SCP.Code qualified
import Tool.SCP.TL qualified
import Tool.SL01 qualified
import Tool.Flowchart qualified

main :: IO ()
main = execParserWithDefaults desc pCmd >>= \case
  CmdSCP scpCmd ->
    case scpCmd of
      CmdSCPReplace cfg -> Tool.SCP.Replace.run    cfg
      CmdSCPEncode  cfg -> Tool.SCP.Code.runEncode cfg
      CmdSCPDecode  cfg -> Tool.SCP.Code.runDecode cfg
      CmdSCPToSCPTL cfg -> Tool.SCP.TL.runToSCPTL  cfg
  CmdSL01 sl01Cmd ->
    case sl01Cmd of
      CmdSL01Compress   cfg -> Tool.SL01.runCompress   cfg
      CmdSL01Decompress cfg -> Tool.SL01.runDecompress cfg
  CmdFlowchart flowchartCmd ->
    case flowchartCmd of
      CmdFlowchartEncode cfg -> Tool.Flowchart.runEncode cfg
      CmdFlowchartDecode cfg -> Tool.Flowchart.runDecode cfg
  where desc = "Various tools for working with GTVM assets."

data Cmd
  = CmdSCP       CmdSCP
  | CmdSL01      CmdSL01
  | CmdFlowchart CmdFlowchart
    deriving (Eq, Show, Generic)

data CmdSCP
  = CmdSCPEncode  Tool.SCP.Code.CfgEncode
  | CmdSCPDecode  Tool.SCP.Code.CfgDecode
  | CmdSCPReplace Tool.SCP.Replace.Cfg
  | CmdSCPToSCPTL Tool.SCP.TL.CfgToSCPTL
    deriving (Eq, Show, Generic)

data CmdSL01
  = CmdSL01Compress   Tool.SL01.CfgCompress
  | CmdSL01Decompress Tool.SL01.CfgDecompress
    deriving (Eq, Show, Generic)

data CmdFlowchart
  = CmdFlowchartEncode Tool.Flowchart.CfgEncode
  | CmdFlowchartDecode Tool.Flowchart.CfgDecode
    deriving (Eq, Show, Generic)

pCmd :: Parser Cmd
pCmd = hsubparser $
       cmd "scp"       descSCP       (CmdSCP       <$> pCmdSCP)
    <> cmd "sl01"      descSL01      (CmdSL01      <$> pCmdSL01)
    <> cmd "flowchart" descFlowchart (CmdFlowchart <$> pCmdFlowchart)
  where
    descSCP       = "Game script file (SCP, script/*.scp) tools."
    descSL01      = "SL01 (LZO1x-compressed file) tools."
    descFlowchart = "flow_chart.bin tools."

pCmdSCP :: Parser CmdSCP
pCmdSCP = hsubparser $
       cmd "encode"   descEncode  (CmdSCPEncode  <$> Tool.SCP.Code.parseCLIOptsEncode)
    <> cmd "decode"   descDecode  (CmdSCPDecode  <$> Tool.SCP.Code.parseCLIOptsDecode)
    <> cmd "replace"  descReplace (CmdSCPReplace <$> Tool.SCP.Replace.parseCLIOpts)
    <> cmd "to-scptl" descToSCPTL (CmdSCPToSCPTL <$> Tool.SCP.TL.parseCLIOptsToSCPTL)
  where
    descEncode  = "Encode YAML SCP to binary."
    descDecode  = "Decode binary SCP to YAML."
    descReplace = "TO REMOVE. Replace textboxes (e.g. for translation)."
    descToSCPTL = "Filter SCP to SCPTL (for translation)."

pCmdSL01 :: Parser CmdSL01
pCmdSL01 = hsubparser $
       cmd "compress"   descCompress   (CmdSL01Compress   <$> Tool.SL01.parseCLIOptsCompress)
    <> cmd "decompress" descDecompress (CmdSL01Decompress <$> Tool.SL01.parseCLIOptsDecompress)
  where
    descCompress   = "Compress game data."
    descDecompress = "Decompress game data."

pCmdFlowchart :: Parser CmdFlowchart
pCmdFlowchart = hsubparser $
       cmd "encode" descEncode (CmdFlowchartEncode <$> Tool.Flowchart.parseCLIOptsEncode)
    <> cmd "decode" descDecode (CmdFlowchartDecode <$> Tool.Flowchart.parseCLIOptsDecode)
  where
    descEncode = "TODO encode"
    descDecode = "TODO decode"

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs showHelpOnError)
    (info (helper <*> p) (progDesc desc))

-- | Shorthand for the way I always write commands.
cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd name desc p = command name (info p (progDesc desc))
