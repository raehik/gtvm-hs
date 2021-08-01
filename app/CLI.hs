module CLI where

import           Config
import           Options.Applicative

pCfgBinaryProcessing :: Parser CfgBinaryProcessing
pCfgBinaryProcessing =
    CfgBinaryProcessing
    <$> pActionDirection
    <*> strArgument (metavar "FILE" <> help "file to work on")
    <*> pOptOutFilepath
    <*> switch (long "print-binary" <> help "allow printing binary to stdout")

pToolGroup :: Parser ToolGroup
pToolGroup = hsubparser $
    command "flowchart" piTGFlowchartCfg
    <> command "scp" piTGSCPCfg

piTGSCPCfg :: ParserInfo ToolGroup
piTGSCPCfg = info (TGSCP <$> pTGSCPCfg) (progDesc desc)
  where desc = "Game script file (SCP, script/*.scp) tools."

pTGSCPCfg :: Parser TGSCPCfg
pTGSCPCfg = TGSCPCfg <$> pCfgBinaryProcessing

piTGFlowchartCfg :: ParserInfo ToolGroup
piTGFlowchartCfg = info (TGFlowchart <$> pTGFlowchartCfg) (progDesc desc)
  where desc = "flow_chart.bin tools."

pTGFlowchartCfg :: Parser TGFlowchartCfg
pTGFlowchartCfg = TGFlowchartCfg <$> pCfgBinaryProcessing <*> pCfgFlowchartType

pActionDirection :: Parser ActionDirection
pActionDirection = pEncode <|> pDecode
  where
    pEncode = flag' ActionDirectionEncode (long "encode" <> help "Encode JSON to binary")
    pDecode = flag' ActionDirectionDecode (long "decode" <> help "Decode binary to JSON")

pCfgFlowchartType :: Parser CfgFlowchartType
pCfgFlowchartType = flag CfgFlowchartTypeParse CfgFlowchartTypeLex
        (long "lex" <> help "Operate on lexed flowcharts (instead of fully parsed)")

pOptOutFilepath :: Parser (Maybe FilePath)
pOptOutFilepath = optional $ strOption $
    long "write-file" <> help "write to file instead of stdout"

parseCLIOpts :: IO ToolGroup
parseCLIOpts = execParserWithDefaults desc pToolGroup
  where
    desc = "Various GTVM tools."

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: String -> Parser a -> IO a
execParserWithDefaults desc p = customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))
