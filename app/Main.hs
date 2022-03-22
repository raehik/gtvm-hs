-- | TODO deforestize plz...

module Main where

import GHC.Generics
import Control.Monad.IO.Class
import Options.Applicative
import Tool.SCP.Code qualified
import Tool.SCP.TL qualified
import Tool.SL01 qualified
import Tool.Flowchart qualified

main :: IO ()
main = execParserWithDefaults desc pCmd >>= \case
  CmdSCP scpCmd ->
    case scpCmd of
      CmdSCPEncode  cfg -> Tool.SCP.Code.runEncode cfg
      CmdSCPDecode  cfg -> Tool.SCP.Code.runDecode cfg
      CmdSCPTL      scptlCmd ->
        case scptlCmd of
          CmdSCPTLGenerate cfg -> Tool.SCP.TL.runToSCPTL    cfg
          CmdSCPTLApply    cfg -> Tool.SCP.TL.runApplySCPTL cfg
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
  = CmdSCPEncode Tool.SCP.Code.CfgEncode
  | CmdSCPDecode Tool.SCP.Code.CfgDecode
  | CmdSCPTL     CmdSCPTL
    deriving (Eq, Show, Generic)

data CmdSCPTL
  = CmdSCPTLGenerate Tool.SCP.TL.CfgToSCPTL
  | CmdSCPTLApply    Tool.SCP.TL.CfgApplySCPTL
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
       cmd "encode"  descEncode  (CmdSCPEncode  <$> Tool.SCP.Code.parseCLIOptsEncode)
    <> cmd "decode"  descDecode  (CmdSCPDecode  <$> Tool.SCP.Code.parseCLIOptsDecode)
    <> cmd "tl"      descTL      (CmdSCPTL      <$> pCmdSCPTL)
  where
    descEncode  = "Encode YAML SCP to binary."
    descDecode  = "Decode binary SCP to YAML."
    descTL      = "SCPTL (SCP translation file) tools."

pCmdSCPTL :: Parser CmdSCPTL
pCmdSCPTL = hsubparser $
       cmd "generate" descGen   (CmdSCPTLGenerate <$> Tool.SCP.TL.parseCLIOptsToSCPTL)
    <> cmd "apply"    descApply (CmdSCPTLApply    <$> Tool.SCP.TL.parseCLIOptsApplySCPTL)
  where
    descGen   = "Generate a template SCPTL from a YAML SCP."
    descApply = "Apply a SCPTL to a given YAML SCP."

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
