module CLI where

import           Config
import           Options.Applicative

pToolGroup :: Parser ToolGroup
pToolGroup = hsubparser $
    command "flowchart" piTGFlowchartCfg

piTGFlowchartCfg :: ParserInfo ToolGroup
piTGFlowchartCfg =
    info
        (TGFlowchart <$> pTGFlowchartCfg)
        (progDesc desc)
  where
    desc = "flow_chart.bin tools."

pTGFlowchartCfg :: Parser TGFlowchartCfg
pTGFlowchartCfg =
    TGFlowchartCfg
    <$> pActionDirection
    <*> pOptFlowchartType
    <*> strArgument (metavar "FILE" <> help "file to work on")

pActionDirection :: Parser ActionDirection
pActionDirection = pEncode <|> pDecode
  where
    pEncode = flag' ActionDirectionEncode (long "encode" <> help "Encode JSON to binary")
    pDecode = flag' ActionDirectionDecode (long "decode" <> help "Decode binary to JSON")

pOptFlowchartType :: Parser CfgFlowchartType
pOptFlowchartType =
    flag CfgFlowchartTypeParse CfgFlowchartTypeLex
        (long "lex" <> help "Operate on lexed flowcharts (instead of fully parsed)")

parseCLIOpts :: IO ToolGroup
parseCLIOpts = execParserWithDefaults desc pToolGroup
  where
    desc = "Various GTVM tools."

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: String -> Parser a -> IO a
execParserWithDefaults desc p =
    customExecParser
        (prefs $ showHelpOnError)
        (info (helper <*> p) (progDesc desc))
