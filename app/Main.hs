{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Config
import           CLI
import           Util

import qualified GTVM.Assorted.Flowchart as GAFc
import qualified GTVM.Assorted.SL01      as GAS
import qualified GTVM.Assorted.Pak       as GAP
import qualified GTVM.Common.Binary.Util as GCBU
import qualified GTVM.Common.Binary.Parse as GCBP
import qualified GTVM.Common.Binary      as GCB
import qualified GTVM.SCP.Parse          as GSP
import qualified GTVM.SCP.Serialize      as GSS

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as DAEP
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BSC
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Data.Text                (Text)
import qualified Data.List                as List

import           Control.Monad ( mapM )
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens

import           System.IO ( stderr, hPutStrLn )
import qualified System.FilePath.Find as Filemanip

import           Text.Megaparsec ( ParsecT, ShowErrorComponent )

main :: IO ()
main = parseCLIOpts >>= runCmd

runCmd :: ToolGroup -> IO ()
runCmd = \case
  TGSCP cfg  -> runCmdSCP cfg
  TGSL01 cfg -> runCmdSL01 cfg
  -- TGFlowchart cfg parseType -> runCmdFlowchart cfg parseType
  TGPak cfg  -> runCmdPak cfg

runCmdSCP :: MonadIO m => CJSON -> m ()
runCmdSCP c = readBytes >>= writeBytes
  where
    readBytes = rReadBytes fromOrig cSFrom cDir
    writeBytes = rWriteStreamFromBinToText cDir cPrintStdout cSTo
    fromOrig fp bs = rEncodeJSON cPrettify <$> GCBP.parseBin GSP.pSCP GCB.binCfgSCP fp bs
    cPrettify = _cJSONPrettify c
    cBin = _cJSONCBin c
    cDir = _cBinCDirection cBin
    cPrintStdout = _cBinAllowBinStdout cBin
    cSFrom = _cBinCStreamFrom cBin
    cSTo   = _cBinCStreamTo cBin

runCmdSL01 :: MonadIO m => CBin -> m ()
runCmdSL01 c = readBytes cDir >>= writeBytes
  where
    readBytes = \case
      CDirectionFromOrig -> rParseStream fromOrig cSFrom
      CDirectionToOrig   -> do
        bytes <- rReadStream cSFrom
        return $ GAS.sSL01 (GAS.compress bytes) GCB.binCfgSCP
    writeBytes = rWriteStreamBin cPrintStdout cSTo
    fromOrig fp bs = do
        sl01 <- GCBP.parseBin GAS.pSL01 GCB.binCfgSCP fp bs
        return $ GAS.decompress sl01
    cDir = _cBinCDirection c
    cPrintStdout = _cBinAllowBinStdout c
    cSFrom = _cBinCStreamFrom c
    cSTo   = _cBinCStreamTo c

runCmdPak :: (MonadIO m) => CPak -> m ()
runCmdPak c =
    case _cPakCDirection c of
      CDirectionFromOrig -> do
        pak <- rParseStream parseAndExtract (_cS1N1 cS1N)
        case _cS1NN cS1N of
          CStreamsArchive fp -> error "unimplemented: write pak to archive"
          CStreamsFolder fp  -> rWritePakFolder pak fp
      CDirectionToOrig   -> do
        case _cS1NN cS1N of
          CStreamsArchive fp -> error "unimplemented: write pak from archive"
          CStreamsFolder fp  -> do
            files <- liftIO $ getDirContentsWithFilenameRecursive fp
            let unk = 0x00200020 -- TODO magic number
                pak = GAP.Pak unk files
                pakBytes = GAP.sPak pak GCB.binCfgSCP
            rWriteStreamBin cPrintStdout (_cS1N1 cS1N) pakBytes
  where
    parseAndExtract :: FilePath -> BS.ByteString -> Either String GAP.Pak
    parseAndExtract fp bs = do
        pakHeader <- GCBP.parseBin GAP.pPakHeader GCB.binCfgSCP fp bs
        return $ pakExtract bs pakHeader
    cS1N = _cPakCS1N c
    cPrintStdout = _cPakAllowBinStdout c

-- Only gets stuff with content (files).
getDirContentsWithFilenameRecursive :: FilePath -> IO [(Text, BS.ByteString)]
getDirContentsWithFilenameRecursive fp = do
    fileList <- Filemanip.find (pure True) (Filemanip.fileType Filemanip.==? Filemanip.RegularFile) fp
    mapM f fileList
  where
    f fp' = do
        bs <- BS.readFile fp'
        let Just fp'' = List.stripPrefix (fp <> "/") fp'
        return (Text.pack fp'', bs)

pakExtract :: BS.ByteString -> GAP.PakHeader -> GAP.Pak
pakExtract bs (GAP.PakHeader unk ft) = GAP.Pak unk (pakExtractFile bs <$> ft)

rReadBytes
    :: MonadIO m
    => (FilePath -> BS.ByteString -> Either String BS.ByteString)
    -> CStream -> CDirection -> m BS.ByteString
rReadBytes parser cSFrom = \case
  CDirectionFromOrig -> rParseStream parser cSFrom
  CDirectionToOrig   -> rReadStream cSFrom

-- | Write bytes to the given stream, choosing whether to use a text or byte
--   print approach depending on 'CDirection'.
--
-- Intended for printing results from orig binary<->text processes. Bin->text
-- means we can always print to stdout; text->bin means we check a flag before
-- printing to stdout.
rWriteStreamFromBinToText :: MonadIO m => CDirection -> Bool -> CStream -> BS.ByteString -> m ()
rWriteStreamFromBinToText dir printStdout =
    case dir of
      CDirectionFromOrig -> rWriteStreamBin True
      CDirectionToOrig   -> rWriteStreamBin printStdout

rWriteStreamBin :: MonadIO m => Bool -> CStream -> BS.ByteString -> m ()
rWriteStreamBin printStdout s bs =
    case s of
      CStreamFile fp -> liftIO $ BS.writeFile fp bs
      CStreamStd     ->
        case printStdout of
          True -> liftIO $ BS.putStr bs
          False -> do
            liftIO $ putStrLn "warning: refusing to print binary to stdout"
            liftIO $ putStrLn "(write to a file with --out-file FILE, or use --print-binary flag to override)"

rEncodeJSON :: Aeson.ToJSON a => Bool -> a -> BS.ByteString
rEncodeJSON prettify =
    case prettify of
      True  -> BL.toStrict . DAEP.encodePretty' prettyCfg
      False -> BL.toStrict . Aeson.encode
  where
    prettyCfg = DAEP.defConfig
      { DAEP.confIndent = DAEP.Spaces 2
      , DAEP.confTrailingNewline = True }

-- TODO: bad error...
rParseStream :: MonadIO m => (FilePath -> BS.ByteString -> Either String a) -> CStream -> m a
rParseStream f s = do
    getStream s >>= \case
      Left  err -> liftIO (putStr err) >> error "fuck"
      Right out -> return out
  where
    getStream = \case
      CStreamFile fp -> f fp        <$> liftIO (BS.readFile fp)
      CStreamStd     -> f "<stdin>" <$> liftIO BS.getContents

--------------------------------------------------------------------------------

rWritePakFolder :: MonadIO m => GAP.Pak -> FilePath -> m ()
rWritePakFolder (GAP.Pak _ files) fp =
    let files' = map (\(a, b) -> (Text.unpack a, b)) files
     in liftIO $ rWriteFilesToFolder fp files'

rWriteFilesToFolder :: MonadIO m => FilePath -> [(FilePath, BS.ByteString)] -> m ()
rWriteFilesToFolder folder = mapM_ f
  where
    f (filename, bytes) = do
        let fp = folder <> "/" <> filename
        liftIO $ putStrLn $ "writing file: " <> fp
        liftIO $ BS.writeFile fp bytes

rReadStream :: MonadIO m => CStream -> m BS.ByteString
rReadStream = \case
  CStreamFile fp -> liftIO $ BS.readFile fp
  CStreamStd     -> liftIO $ BS.getContents

-- | Includes the filepath (to use in debug info).
rProcessStream' :: MonadIO m => (FilePath -> BS.ByteString -> a) -> CStream -> m a
rProcessStream' p = \case
  CStreamFile fp -> p fp <$> liftIO (BS.readFile fp)
  CStreamStd     -> p "" <$> liftIO BS.getContents

rProcessStream :: MonadIO m => (BS.ByteString -> a) -> CStream -> m a
rProcessStream p = \case
  CStreamFile fp -> p <$> liftIO (BS.readFile fp)
  CStreamStd     -> p <$> liftIO BS.getContents

--------------------------------------------------------------------------------

{-

rParseFile
    :: (MonadReader env m, HasCfgBinIO env, MonadIO m, ShowErrorComponent e)
    => GCB.BinaryCfg
    -> (ParsecT e BS.ByteString (Reader GCB.BinaryCfg) a)
    -> m (Either String a)
rParseFile cfg p = do
    fp <- asks' $ cfgBinIO . cfgBinIOFilepath
    GCBU.runParserBinFile p fp cfg

rParseFile'
    :: (MonadReader env m, HasCfgBinIO env, MonadIO m, ShowErrorComponent e)
    => (ParsecT e BS.ByteString (Reader GCB.BinaryCfg) a)
    -> m (Either String a)
rParseFile' = rParseFile GCB.binCfgSCP

rBinOutputBytes
    :: (MonadReader env m, HasCfgBinIO env, MonadIO m)
    => BL.ByteString -> m ()
rBinOutputBytes bs = do
    cfg <- asks' cfgBinIO
    case _cfgBinIOOutFilepath cfg of
      Just outFp -> liftIO $ BL.writeFile outFp bs
      Nothing -> do
        if   _cfgBinIODirection cfg == ActionDirectionDecode
        then liftIO $ BL.putStr bs >> putStrLn ""
        else
            case _cfgBinIOAllowBinaryOnStdout cfg of
              True -> liftIO $ BL.putStr bs
              False -> do
                liftIO $ putStrLn "warning: refusing to print binary to stdout"
                liftIO $ putStrLn "(write to a file with --write-file FILE, or use --print-binary flag to override)"

rBinDecodeJsonAndReserialize
    :: (MonadReader env m, HasCfgBinJSON env, MonadIO m, Aeson.FromJSON a)
    => (a -> BS.ByteString) -> m BS.ByteString
rBinDecodeJsonAndReserialize serialize = do
    cfg <- asks' $ cfgBinJSON . cfgBinIO
    let fp = _cfgBinIOFilepath cfg
    bytes <- liftIO $ BL.readFile fp
    case Aeson.eitherDecode bytes of
      Left err      -> liftIO (putStrLn $ "JSON decoding error: " <> err) >>= error "fuck"
      Right decoded -> return $ serialize decoded

rEncodeJSON
    :: (MonadReader env m, HasCfgBinJSON env, MonadIO m, Aeson.ToJSON a)
    => a -> m BL.ByteString
rEncodeJSON a = do
    cfg <- asks' cfgBinJSON
    if   _cfgBinJSONPrettify cfg
    then return $ DAEP.encodePretty' prettyCfg a
    else return $ Aeson.encode a
  where prettyCfg = DAEP.defConfig { DAEP.confIndent = DAEP.Spaces 2 }

runCmdFlowchartDecode :: (MonadReader TGFlowchartCfg m, MonadIO m) => m BL.ByteString
runCmdFlowchartDecode = do
    lexed <- rParseFile' GAFc.pFlowchart
    case lexed of
      Left err      -> liftIO (putStr err) >> error "fuck"
      Right lexed' -> do
        ty <- asks' tgFlowchartCfgType
        case ty of
          CfgFlowchartTypeParse -> do
            let parsed = GAFc.fcToAltFc lexed'
            rEncodeJSON parsed
          CfgFlowchartTypeLex -> rEncodeJSON lexed'

runCmdFlowchartEncode :: (MonadReader TGFlowchartCfg m, MonadIO m) => m BS.ByteString
runCmdFlowchartEncode = do
    liftIO $ hPutStrLn stderr "Ignoring flowchart type SRY"
    rBinDecodeJsonAndReserialize $ \fc -> GAFc.sFlowchart (GAFc.altFcToFc fc) GCB.binCfgSCP

rBinActionAndOutput
    :: (MonadReader env m, HasCfgBinIO env, MonadIO m)
    => m BL.ByteString -> m BL.ByteString -> m ()
rBinActionAndOutput fDe fEn = do
    cfg <- asks' cfgBinIO
    case _cfgBinIODirection cfg of
      ActionDirectionDecode -> fDe >>= rBinOutputBytes
      ActionDirectionEncode -> fEn >>= rBinOutputBytes

runCmdFlowchart :: (MonadReader TGFlowchartCfg m, MonadIO m) => m ()
runCmdFlowchart = rBinActionAndOutput fDe fEn
  where
    fDe = runCmdFlowchartDecode
    fEn = BL.fromStrict <$> runCmdFlowchartEncode

-}

pakExtractFile :: BS.ByteString -> GAP.PakHeaderFTE -> (Text, BS.ByteString)
pakExtractFile bs (GAP.PakHeaderFTE offset len filenameBS) =
    let fileBs   = bsExtract (fromIntegral offset) (fromIntegral len) bs
        filename = Text.decodeUtf8 filenameBS
     in (filename, fileBs)

bsExtract :: Int -> Int -> BS.ByteString -> BS.ByteString
bsExtract offset len bs =
    let bs' = (BS.take len . BS.drop offset) bs
     in if   BS.length bs' /= len
        then error "ya bytes fucked"
        else bs'
