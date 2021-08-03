{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Config
import           CLI
import           Util

import qualified GTVM.Assorted.Flowchart as GAFc
import qualified GTVM.Assorted.SL01      as GAS
import qualified GTVM.Assorted.Pak       as GAP
import qualified GTVM.Common.Binary.Util as GCBU
import qualified GTVM.Common.Binary      as GCB
import qualified GTVM.SCP.Parse          as GSP
import qualified GTVM.SCP.Serialize      as GSS

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as DAEP
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as BS
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Data.Text                (Text)

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Lens

import           System.IO ( stderr, hPutStrLn )

import           Text.Megaparsec ( ParsecT, ShowErrorComponent )

main :: IO ()
main = parseCLIOpts >>= runCmd

runCmd :: ToolGroup -> IO ()
runCmd = \case
  TGSCP (TGSCPCfg cfg) -> runReaderT runCmdSCP cfg
  TGSL01 (TGSL01Cfg cfg) -> runReaderT runCmdSL01 cfg
  TGFlowchart cfg -> runReaderT runCmdFlowchart cfg
  TGPak cfg -> runReaderT runCmdPak cfg

runCmdSL01 :: (MonadReader env m, HasCfgBinIO env, MonadIO m) => m ()
runCmdSL01 = rBinActionAndOutput fDe fEn
  where
    fDe = do
        rParseFile' GAS.pSL01 >>= \case
          Left err     -> liftIO (putStr err) >> error "fuck"
          Right parsed -> return . BL.fromStrict . GAS.decompress $ parsed
    fEn = do
        cfg <- asks' cfgBinIO
        let fp = _cfgBinIOFilepath cfg
        bytes <- liftIO $ BL.readFile fp
        let sl01 = GAS.compress (BL.toStrict bytes)
            sl01Bytes = GAS.sSL01 sl01 GCB.binCfgSCP
        return (BL.fromStrict sl01Bytes)

rBinActionAndOutput
    :: (MonadReader env m, HasCfgBinIO env, MonadIO m)
    => m BL.ByteString -> m BL.ByteString -> m ()
rBinActionAndOutput fDe fEn = do
    cfg <- asks' cfgBinIO
    case _cfgBinIODirection cfg of
      ActionDirectionDecode -> fDe >>= rBinOutputBytes
      ActionDirectionEncode -> fEn >>= rBinOutputBytes

runCmdSCP :: (MonadReader CfgBinJSON m, MonadIO m) => m ()
runCmdSCP = rBinActionAndOutput fDe fEn
  where
    fDe = runCmdScpDecode
    fEn = BL.fromStrict <$> runCmdScpEncode

runCmdScpDecode :: (MonadReader CfgBinJSON m, MonadIO m) => m BL.ByteString
runCmdScpDecode = do
    rParseFile' GSP.pSCP >>= \case
      Left err     -> liftIO (putStr err) >> error "fuck"
      Right parsed -> rEncodeJSON parsed

runCmdScpEncode :: (MonadReader CfgBinJSON m, MonadIO m) => m BS.ByteString
runCmdScpEncode = rBinDecodeJsonAndReserialize (flip GSS.sSCP GCB.binCfgSCP)

runCmdFlowchart :: (MonadReader TGFlowchartCfg m, MonadIO m) => m ()
runCmdFlowchart = rBinActionAndOutput fDe fEn
  where
    fDe = runCmdFlowchartDecode
    fEn = BL.fromStrict <$> runCmdFlowchartEncode

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

rEncodeJSON
    :: (MonadReader env m, HasCfgBinJSON env, MonadIO m, Aeson.ToJSON a)
    => a -> m BL.ByteString
rEncodeJSON a = do
    cfg <- asks' cfgBinJSON
    if   _cfgBinJSONPrettify cfg
    then return $ DAEP.encodePretty' prettyCfg a
    else return $ Aeson.encode a
  where prettyCfg = DAEP.defConfig { DAEP.confIndent = DAEP.Spaces 2 }

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

rDirectionCase
    :: (MonadReader env m, HasCDirection env)
    => m a -> m a -> m a
rDirectionCase fFrom fTo = asks' cDirection >>= \case
  CDirectionFromOrig -> fFrom
  CDirectionToOrig   -> fTo

runCmdPak :: (MonadReader env m, HasCPak env, HasCDirection env, MonadIO m) => m ()
runCmdPak = rDirectionCase fDe (error "pak encoding unimplemented")
  where
    fDe = do
        cfg <- asks' cPak
        case cfg ^. cPakCS1 of
          CStreamStd     -> error "stdin usage unimplemented"
          CStreamFile fp -> do
            GCBU.runParserBinFile GAP.pPakHeader fp GCB.binCfgSCP >>= \case
              Left err     -> liftIO (putStr err) >> error "fuck"
              Right parsed -> do
                (GAP.Pak unk files) <- tgPakExtractFiles fp parsed
                case cfg ^. cPakCS2s of
                  CStreamsFolder  fp' -> do
                    let files' = map (\(a, b) -> (Text.unpack a, b)) files
                     in liftIO $ rWriteFilesToFolder fp' files'
                  CStreamsArchive fp' -> error $ "out to archive: " <> fp'

rWriteFilesToFolder :: MonadIO m => FilePath -> [(FilePath, BS.ByteString)] -> m ()
rWriteFilesToFolder folder = mapM_ f
  where
    f (filename, bytes) = do
        let fp = folder <> "/" <> filename
        liftIO $ putStrLn $ "writing file: " <> fp
        liftIO $ BS.writeFile fp bytes

tgPakExtractFiles :: MonadIO m => FilePath -> GAP.PakHeader -> m GAP.Pak
tgPakExtractFiles fp (GAP.PakHeader unk ft) = do
    bs <- liftIO $ BS.readFile fp
    let files = tgPakExtractFile bs <$> ft
    return $ GAP.Pak unk files

tgPakExtractFile :: BS.ByteString -> GAP.PakHeaderFTE -> (Text, BS.ByteString)
tgPakExtractFile bs (GAP.PakHeaderFTE offset len filenameBS) =
    let fileBs   = bsExtract (fromIntegral offset) (fromIntegral len) bs
        filename = Text.decodeUtf8 filenameBS
     in (filename, fileBs)

bsExtract :: Int -> Int -> BS.ByteString -> BS.ByteString
bsExtract offset len bs =
    let bs' = (BS.take len . BS.drop offset) bs
     in if   BS.length bs' /= len
        then error "ya bytes fucked"
        else bs'
