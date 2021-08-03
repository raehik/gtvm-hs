{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Config
import           CLI
import           Util

import qualified GTVM.Assorted.Flowchart as GAFc
import qualified GTVM.Assorted.SL01      as GAS
import qualified GTVM.Common.Binary.Util as GCBU
import qualified GTVM.Common.Binary      as GCB
import qualified GTVM.SCP.Parse          as GSP
import qualified GTVM.SCP.Serialize      as GSS

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as DAEP
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as BS

import           Control.Monad.IO.Class
import           Control.Monad.Reader

import           System.IO ( stderr, hPutStrLn )

import           Text.Megaparsec ( ParsecT, ShowErrorComponent )

main :: IO ()
main = parseCLIOpts >>= runCmd

runCmd :: ToolGroup -> IO ()
runCmd = \case
  TGFlowchart cfg -> runReaderT runCmdFlowchart cfg
  TGSCP (TGSCPCfg cfg) -> runReaderT runCmdSCP cfg
  TGSL01 (TGSL01Cfg cfg) -> runReaderT runCmdSL01 cfg

runCmdSL01 :: (MonadReader CfgBinIO m, MonadIO m) => m ()
runCmdSL01 = rBinActionAndOutput fDe fEn
  where
    fDe = do
        rParseFile GAS.pSL01 GCB.binCfgSCP >>= \case
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
    rParseFile GSP.pSCP GCB.binCfgSCP >>= \case
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
    lexed <- rParseFile GAFc.pFlowchart GCB.binCfgSCP
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
    => (ParsecT e BS.ByteString (Reader GCB.BinaryCfg) a)
    -> GCB.BinaryCfg -> m (Either String a)
rParseFile p cfg = do
    fp <- asks' $ cfgBinIO . cfgBinIOFilepath
    GCBU.runParserBinFile p fp cfg

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
