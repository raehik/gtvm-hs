module CLI where

import           Config
import           Options.Applicative
import qualified Data.Char as Char
import           Control.Monad.IO.Class

parseCLIOpts :: MonadIO m => m ToolGroup
parseCLIOpts = execParserWithDefaults desc pToolGroup
  where
    desc = "Various GTVM tools."

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

pToolGroup :: Parser ToolGroup
pToolGroup = hsubparser $
    makeCmd "flowchart" descFlowchart (TGFlowchart <$> pCJSON <*> pCParseType)
    <> makeCmd "scp"    descSCP       (TGSCP <$> pCJSON)
    <> makeCmd "sl01"   descSL01      (TGSL01 <$> pCBin pCompressDirection)
    <> makeCmd "pak"    descPak       (TGPak <$> pCPak)
  where
    makeCmd name desc p = command name (info p (progDesc desc))
    descSCP = "Game script file (SCP, script/*.scp) tools."
    descSL01 = "SL01 (LZO1x-compressed file) tools."
    descPak = ".pak (sound_se.pak) tools."
    descFlowchart = "flow_chart.bin tools."

pCParseType :: Parser CParseType
pCParseType = flag CParseTypeFull CParseTypePartial
        (long "lex" <> help "Operate on simply-parsed data (instead of fully parsed)")

--------------------------------------------------------------------------------

capitalize :: String -> String
capitalize = \case
  []   -> []
  c:cs -> Char.toUpper c : cs

pCDirection :: String -> String -> String -> String -> Parser CDirection
pCDirection verbFrom helpFrom verbTo helpTo = pFrom <|> pTo
  where
    pFrom = flag' CDirectionFromOrig (long verbFrom <> help helpFrom')
    pTo   = flag' CDirectionToOrig   (long verbTo   <> help helpTo')
    helpFrom' = capitalize verbFrom <> " " <> helpFrom
    helpTo'   = capitalize verbTo   <> " " <> helpTo

pBinCodingDirection :: String -> Parser CDirection
pBinCodingDirection toNoun = pCDirection "decode" helpFrom "encode" helpTo
  where
    helpFrom = "binary -> " <> toNoun
    helpTo   = toNoun <> " -> binary"

-- TODO: Make an alternate command-based parser.
pPackDirection :: Parser CDirection
pPackDirection = pCDirection "unpack" "archive" "pack" "archive"

pCompressDirection :: Parser CDirection
pCompressDirection = pCDirection "decompress" "file" "compress" "file"

pCStream :: CDirection -> Parser CStream
pCStream = \case
  CDirectionFromOrig -> pFileArg <|> pStdin
  CDirectionToOrig   -> pFileOpt <|> pStdout
  where
    pFileArg = CStreamFile <$> strArgument (metavar "FILE" <> help "Input file")
    pFileOpt = CStreamFile <$> strOption (metavar "FILE" <> long "out-file" <> help "Output file")
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")
    pStdout  = flag' CStreamStd (long "stdout" <> help "Use stdout")

pCStreams :: CDirection -> Parser CStreams
pCStreams dir = pFolder <|> pArchive
  where
    pFolder  = CStreamsFolder  <$> strOption (metavar "FOLDER" <> long (dir' <> "-folder")  <> help (dir'' <> " folder"))
    pArchive = CStreamsArchive <$> strOption (metavar "FILE" <> long (dir' <> "-archive") <> help (dir'' <> " archive"))
    dir'  = cDirectionCaseFromTo "in"    "out"    dir
    dir'' = cDirectionCaseFromTo "Input" "Output" dir

pCBin :: Parser CDirection -> Parser CBin
pCBin pDir =
    CBin
    <$> pDir
    <*> pCStream CDirectionFromOrig
    <*> pCStream CDirectionToOrig
    <*> pAllowBinStdout

pCJSON :: Parser CJSON
pCJSON = CJSON <$> pCBin (pBinCodingDirection "JSON") <*> pPrettifyOrNo
  where pPrettifyOrNo = switch (long "prettify" <> help "Prettify JSON")

pAllowBinStdout :: Parser Bool
pAllowBinStdout = switch (long "print-binary" <> help "Allow printing binary to stdout")

pCS1N :: CDirection -> CDirection -> Parser CS1N
pCS1N d1 d2 = CS1N <$> pCStream d1 <*> pCStreams d2

pCPak :: Parser CPak
pCPak = hsubparser $
    command "unpack" (info (CPak <$> pure CDirectionFromOrig <*> pCS1N CDirectionFromOrig CDirectionToOrig <*> pAllowBinStdout) (progDesc "Unpack archive."))
    <> command "pack" (info (CPak <$> pure CDirectionToOrig <*> pCS1N CDirectionToOrig CDirectionFromOrig <*> pAllowBinStdout) (progDesc "Pack archive."))
