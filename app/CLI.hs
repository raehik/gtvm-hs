module CLI ( parseOpts, ToolGroup(..) ) where

import           GHC.Generics
import           Config
import           Options.Applicative
import           Control.Monad.IO.Class
import qualified Data.Char                  as Char
import           Data.Word
--import qualified CSV                        as CSV
--import           CSV                        ( CSVTextReplace(..) )
import qualified Data.Text                  as Text
import qualified Tool.SCP.Replace
import qualified CLI.Command

data ToolGroup
  = TGFlowchart CJSON (CStream, CStream) CParseType
  | TGSCP CJSON (CStream, CStream)
  | TGSCPX (CStream, CStream)
  | TGSL01 CBin (CStream, CStream)
  | TGPak CPak Bool
  | TGCSVPatch (CStream, CStream)
  | TGSCPReplace Tool.SCP.Replace.Cfg
    deriving (Eq, Show, Generic)

parseOpts :: MonadIO m => m ToolGroup
parseOpts = execParserWithDefaults desc pToolGroup
  where
    desc = "Various GTVM tools."

pToolGroup :: Parser ToolGroup
pToolGroup = hsubparser $
       cmd "scp"    descSCP   (TGSCP  <$> pCJSON "SCP" <*> pCStream2)
    <> cmd "scpx"   descSCPX  (TGSCPX <$> pCStream2)
    <> cmd "sl01"   descSL01  (TGSL01 <$> pCBin <*> pCStream2)
    <> cmd "flowchart" descFlowchart
        (TGFlowchart <$> pCJSON "flow_chart.bin" <*> pCStream2 <*> pCParseType)
    <> cmd "pak"    descPak   (TGPak <$> pCPak <*> pAllowBinStdout)
    <> expandCmd TGSCPReplace Tool.SCP.Replace.cmd
    -- <> cmd' "csv-patch"  descCSVPatch headerCSVPatch (TGCSVPatch <$> pCStream2)
  where
    descSCP       = "Game script file (SCP, script/*.scp) tools."
    descSCPX      = "Convert extended SCP to regular SCP (YAML both ways)."
    descSL01      = "SL01 (LZO1x-compressed file) tools."
    descFlowchart = "flow_chart.bin tools."
    descPak       = ".pak (sound_se.pak) tools."
    descCSVPatch  = "Convert a string patch CSV to an applicable string patch."
    descScpReplace = "Replace textboxes in an SCP."
    pCParseType = flag CParseTypeFull CParseTypePartial
            (long "lex" <> help "Operate on simply-parsed data (instead of fully parsed)")
{-
    headerCSVPatch =
        "Your CSV file must start with a line of headers, which must include the following names: "
        <> csvColName csvSrOffset
        <> ", " <> csvColName csvSrReplText
        <> ", " <> csvColName csvSrAvailableSpace
        <> ", " <> csvColName csvSrSucceedingNulls
        <> ", " <> csvColName csvSrOrigText
    csvColName = Text.unpack . CSV.getColName
-}

pCStreamPair :: Parser CStreamPair
pCStreamPair = CStreamPair <$> pCSIn <*> pCSOut
  where
    pCSIn    = pFileArg <|> pStdin
    pCSOut   = pFileOpt <|> pure CStreamStd
    pFileArg = CStreamFile <$> strArgument (metavar "FILE" <> help "Input file")
    pFileOpt = CStreamFile <$> strOption (metavar "FILE" <> long "out-file" <> short 'o' <> help "Output file")
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")

pCPak :: Parser CPak
pCPak = hsubparser $
       cmd "unpack" descUnpack pUnpack
    <> cmd "pack"   descPack   pPack
  where
    descUnpack = "Unpack a pak archive."
    descPack   = "Pack files to a new pak archive."
    pUnpack = CPakUnpack <$> pCS1N CDirectionFromOrig CDirectionToOrig
    pPack   = CPakPack   <$> pCS1N CDirectionToOrig CDirectionFromOrig <*> pUnk
    pUnk = pW32 "header-unk" "Unknown header value. Note that it's read as big-endian, and doesn't limit to Word32. (TODO)"

pCJSON :: String -> Parser CJSON
pCJSON noun = hsubparser $
       cmd "decode" descDe (CJSONDe <$> pPrettifyJSON)
    <> cmd "encode" descEn (CJSONEn <$> pAllowBinStdout)
  where
    descDe = "Decode " <> noun <> " to JSON."
    descEn = "Encode JSON to " <> noun <> "."

pCBin :: Parser CBin
pCBin = CBin <$> pSub <*> pAllowBinStdout
  where
    pSub = hsubparser $
           cmd "decompress" "Decompress TODO" (pure CDirectionFromOrig)
        <> cmd "compress"   "Compress TODO"   (pure CDirectionToOrig)

pCStreams :: CDirection -> Parser CStreams
pCStreams dir = pFolder <|> pArchive
  where
    pFolder  = CStreamsFolder  <$> strOption (metavar "FOLDER" <> long (dir' <> "-folder")  <> help (dir'' <> " folder"))
    pArchive = CStreamsArchive <$> strOption (metavar "FILE" <> long (dir' <> "-archive") <> help (dir'' <> " archive"))
    (dir', dir'')  =
        case dir of
          CDirectionFromOrig -> ("in", "Input")
          CDirectionToOrig   -> ("out", "Output")

pAllowBinStdout :: Parser Bool
pAllowBinStdout = switch (long "print-binary" <> help "Allow printing binary to stdout")

pPrettifyJSON :: Parser Bool
pPrettifyJSON = switch (long "prettify" <> help "Prettify JSON")

pCS1N :: CDirection -> CDirection -> Parser (CStream, CStreams)
pCS1N d1 d2 = (,) <$> pCStream d1 <*> pCStreams d2

pCStream :: CDirection -> Parser CStream
pCStream = \case
  CDirectionFromOrig -> pCStreamIn "file"
  CDirectionToOrig   -> pCStreamOut "file"

pFileIn :: String -> String -> Parser FilePath
pFileIn meta noun = strArgument (metavar meta <> help ("Input "<>noun))

pCStreamIn :: String -> Parser CStream
pCStreamIn noun = pFileArg <|> pStdin
  where
    pFileArg = CStreamFile <$> strArgument (metavar (map Char.toUpper noun) <> help ("Input " <> noun))
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")

pCStreamOut :: String -> Parser CStream
pCStreamOut noun = pFileOpt <|> pure CStreamStd
  where
    pFileOpt = CStreamFile <$> strOption (metavar (map Char.toUpper noun) <> long "out-file" <> help ("Output " <> noun))

pW32 :: String -> String -> Parser Word32
pW32 noun h = option auto $
    metavar "NUM"
    <> long noun
    <> help (h <> ". 4-byte number (up to 0xFFFFFFFF)")

-- | Defaults to file in, stdout out.
pCStream2 :: Parser (CStream, CStream)
pCStream2 = liftA2 (,) pCSIn pCSOut
  where
    pCSIn    = pFileArg <|> pStdin
    pFileArg = CStreamFile <$> strArgument (metavar "FILE" <> help "Input file")
    pFileOpt = CStreamFile <$> strOption (metavar "FILE" <> long "out-file" <> short 'o' <> help "Output file")
    pStdin   = flag' CStreamStd (long "stdin"  <> help "Use stdin")
    pCSOut   = pFileOpt <|> pure CStreamStd

--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

-- | Shorthand for the way I always write commands.
cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd name desc p = command name (info p (progDesc desc))

cmd' :: String -> String -> String -> Parser a -> Mod CommandFields a
cmd' name desc h p = command name (info p (progDesc desc <> header h))

expandCmd :: (a -> ToolGroup) -> CLI.Command.Cmd a -> Mod CommandFields ToolGroup
expandCmd f c =
    cmd
        (CLI.Command.cmdName c)
        (CLI.Command.cmdDesc c)
        (f <$> CLI.Command.cmdParser c)
