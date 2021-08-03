module CLI where

import           Config
import           Options.Applicative
import qualified Data.Char as Char

pToolGroup :: Parser ToolGroup
pToolGroup = hsubparser $
    command "flowchart" piTGFlowchartCfg
    <> command "scp" piTGSCPCfg
    <> command "sl01" piTGSL01Cfg
    <> command "pak" piTGPak

piTGPak :: ParserInfo ToolGroup
piTGPak = info (TGPak <$> pCfgBinIO) (progDesc desc)
  where desc = ".pak (sound_se.pak) tools."

pCfgBinIO :: Parser CfgBinIO
pCfgBinIO =
    CfgBinIO
    <$> pActionDirection
    <*> strArgument (metavar "FILE" <> help "File to work on")
    <*> pOptOutFilepath
    <*> switch (long "print-binary" <> help "Allow printing binary to stdout")
  where
    pOptOutFilepath =
        optional $ strOption $
            long "write-file" <> help "Write to file instead of stdout"

pCfgBinJSON :: Parser CfgBinJSON
pCfgBinJSON = CfgBinJSON <$> pCfgBinIO <*> pPrettifyOrNo
  where pPrettifyOrNo = pYesOrNo "prettify" "prettify JSON"

piTGFlowchartCfg :: ParserInfo ToolGroup
piTGFlowchartCfg = info (TGFlowchart <$> pTGFlowchartCfg) (progDesc desc)
  where desc = "flow_chart.bin tools."

pTGFlowchartCfg :: Parser TGFlowchartCfg
pTGFlowchartCfg = TGFlowchartCfg <$> pCfgBinJSON <*> pCfgFlowchartType

pCfgFlowchartType :: Parser CfgFlowchartType
pCfgFlowchartType = flag CfgFlowchartTypeParse CfgFlowchartTypeLex
        (long "lex" <> help "Operate on lexed flowcharts (instead of fully parsed)")

piTGSCPCfg :: ParserInfo ToolGroup
piTGSCPCfg = info (TGSCP <$> pTGSCPCfg) (progDesc desc)
  where desc = "Game script file (SCP, script/*.scp) tools."

pTGSCPCfg :: Parser TGSCPCfg
pTGSCPCfg = TGSCPCfg <$> pCfgBinJSON

piTGSL01Cfg :: ParserInfo ToolGroup
piTGSL01Cfg = info (TGSL01 <$> pTGSL01Cfg) (progDesc desc)
  where desc = "SL01 (LZO1x-compressed file) tools."

pTGSL01Cfg :: Parser TGSL01Cfg
pTGSL01Cfg = TGSL01Cfg <$> pCfgBinIO

pActionDirection :: Parser ActionDirection
pActionDirection = pEncode <|> pDecode
  where
    pEncode = flag' ActionDirectionEncode (long "encode" <> help "Encode JSON to binary")
    pDecode = flag' ActionDirectionDecode (long "decode" <> help "Decode binary to JSON")

parseCLIOpts :: IO ToolGroup
parseCLIOpts = execParserWithDefaults desc pToolGroup
  where
    desc = "Various GTVM tools."

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: String -> Parser a -> IO a
execParserWithDefaults desc p = customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

pYesOrNo :: String -> String -> Parser Bool
pYesOrNo verb desc = p1 <|> p2
  where
    p1 = flag True True $ long verb <> help (capitalize desc <> " (default)")
    p2 = flag' False $ long ("no-" <> verb) <> help ("Don't " <> desc)
    capitalize = \case
      []   -> []
      c:cs -> Char.toUpper c : cs
