module CLI (parseOpts) where

import           Config
import           Options.Applicative
import           Control.Monad.IO.Class
import qualified Data.Char as Char

parseOpts :: MonadIO m => m ToolGroup
parseOpts = execParserWithDefaults desc pToolGroup
  where
    desc = "Various GTVM tools."

pToolGroup :: Parser ToolGroup
pToolGroup = hsubparser $
    makeCmd "flowchart" descFlowchart (TGFlowchart <$> pCJSON "flow_chart.bin" <*> pCParseType)
    <> makeCmd "scp"    descSCP       (TGSCP <$> pCJSON "SCP")
    <> makeCmd "sl01"   descSL01      (TGSL01 <$> pCBin)
    <> makeCmd "pak"    descPak       (TGPak <$> pCPak)
  where
    makeCmd name desc p = command name (info p (progDesc desc))
    descSCP = "Game script file (SCP, script/*.scp) tools."
    descSL01 = "SL01 (LZO1x-compressed file) tools."
    descPak = ".pak (sound_se.pak) tools."
    descFlowchart = "flow_chart.bin tools."
    pCParseType = flag CParseTypeFull CParseTypePartial
            (long "lex" <> help "Operate on simply-parsed data (instead of fully parsed)")

pCStream :: CDirection -> Parser CStream
pCStream = \case
  CDirectionFromOrig -> pCStreamIn "file"
  CDirectionToOrig   -> pCStreamOut "file"

pCStreamIn :: String -> Parser CStream
pCStreamIn noun = pFileArg <|> pStdin
  where
    pFileArg = CStreamFile <$> strArgument (metavar (map Char.toUpper noun) <> help ("Input " <> noun))
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")

pCStreamOut :: String -> Parser CStream
pCStreamOut noun = pFileOpt <|> pure CStreamStd
  where
    pFileOpt = CStreamFile <$> strOption (metavar (map Char.toUpper noun) <> long "out-file" <> help ("Output " <> noun))

pCStreams :: CDirection -> Parser CStreams
pCStreams dir = pFolder <|> pArchive
  where
    pFolder  = CStreamsFolder  <$> strOption (metavar "FOLDER" <> long (dir' <> "-folder")  <> help (dir'' <> " folder"))
    pArchive = CStreamsArchive <$> strOption (metavar "FILE" <> long (dir' <> "-archive") <> help (dir'' <> " archive"))
    (dir', dir'')  =
        case dir of
          CDirectionFromOrig -> ("in", "Input")
          CDirectionToOrig   -> ("out", "Output")

pCBin :: Parser CBin
pCBin = hsubparser $
       command "decompress" (info (p CDirectionFromOrig) (progDesc "Decompress TODO"))
    <> command "compress"   (info (p CDirectionToOrig)   (progDesc "Compress TODO"))
  where
    p d = CBin <$> pure d <*> pCStream2 <*> pAllowBinStdout

pAllowBinStdout :: Parser Bool
pAllowBinStdout = switch (long "print-binary" <> help "Allow printing binary to stdout")

pPrettifyJSON :: Parser Bool
pPrettifyJSON = switch (long "prettify" <> help "Prettify JSON")

pCS1N :: CDirection -> CDirection -> Parser CS1N
pCS1N d1 d2 = CS1N <$> pCStream d1 <*> pCStreams d2

pCPak :: Parser CPak
pCPak = hsubparser $
    command "unpack" (info (CPak <$> pure CDirectionFromOrig <*> pCS1N CDirectionFromOrig CDirectionToOrig <*> pAllowBinStdout) (progDesc "Unpack archive."))
    <> command "pack" (info (CPak <$> pure CDirectionToOrig <*> pCS1N CDirectionToOrig CDirectionFromOrig <*> pAllowBinStdout) (progDesc "Pack archive."))

pCJSON :: String -> Parser CJSON
pCJSON noun = hsubparser $
       command "decode" (info (CJSONDe <$> pCStream2 <*> pPrettifyJSON)   (progDesc ("Decode " <> noun <> " to JSON.")))
    <> command "encode" (info (CJSONEn <$> pCStream2 <*> pAllowBinStdout) (progDesc ("Encode JSON to " <> noun <> ".")))

-- | Defaults to file in, stdout out.
pCStream2 :: Parser (CStream, CStream)
pCStream2 = liftA2 (,) pCSIn pCSOut
  where
    pCSIn    = pFileArg <|> pStdin
    pFileArg = CStreamFile <$> strArgument (metavar "FILE" <> help "Input file")
    pFileOpt = CStreamFile <$> strOption (metavar "FILE" <> long "out-file" <> help "Output file")
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")
    pCSOut   = pFileOpt <|> pure CStreamStd

--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))
