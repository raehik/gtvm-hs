{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Config
import           CLI

import qualified GTVM.Assorted.Flowchart as GAFc
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
  TGSCP (TGSCPCfg binProcCfg) -> runReaderT runCmdSCP binProcCfg

runCmdSCP :: (MonadReader CfgBinaryProcessing m, MonadIO m) => m ()
runCmdSCP = do
    asks _cfgBinaryProcessingDirection >>= \case
      ActionDirectionDecode -> runCmdScpDecode >>= rBinOutputBytes
      ActionDirectionEncode -> runCmdScpEncode >>= rBinOutputBytes . BL.fromStrict

runCmdScpDecode :: (MonadReader CfgBinaryProcessing m, MonadIO m) => m BL.ByteString
runCmdScpDecode = do
    rParseFile GSP.pSCP GCB.binCfgSCP >>= \case
      Left err     -> liftIO (putStr err) >> error "fuck"
      Right parsed -> return $ encodeJSON parsed

runCmdScpEncode :: (MonadReader CfgBinaryProcessing m, MonadIO m) => m BS.ByteString
runCmdScpEncode = rBinDecodeJsonAndReserialize (flip GSS.sSCP GCB.binCfgSCP)

runCmdFlowchart :: (MonadReader TGFlowchartCfg m, MonadIO m) => m ()
runCmdFlowchart = do
    binProcCfg <- asks _tgFlowchartCfgBinaryProcessing
    case _cfgBinaryProcessingDirection binProcCfg of
      ActionDirectionDecode -> runCmdFlowchartDecode >>= rBinOutputBytes
      ActionDirectionEncode -> runCmdFlowchartEncode >>= rBinOutputBytes . BL.fromStrict

runCmdFlowchartDecode :: (MonadReader TGFlowchartCfg m, MonadIO m) => m BL.ByteString
runCmdFlowchartDecode = do
    binProcCfg <- asks _tgFlowchartCfgBinaryProcessing
    lexed <- flip runReaderT binProcCfg $ rParseFile GAFc.pFlowchart GCB.binCfgSCP
    case lexed of
      Left err      -> liftIO (putStr err) >> error "fuck"
      Right lexed' -> do
        ty <- asks _tgFlowchartCfgType
        case ty of
          CfgFlowchartTypeParse -> do
            let parsed = GAFc.fcToAltFc lexed'
            return $ encodeJSON parsed
          CfgFlowchartTypeLex -> return $ encodeJSON lexed'

runCmdFlowchartEncode :: (MonadReader TGFlowchartCfg m, MonadIO m) => m BS.ByteString
runCmdFlowchartEncode = do
    liftIO $ hPutStrLn stderr "Ignoring flowchart type SRY"
    rBinDecodeJsonAndReserialize $ \fc -> GAFc.sFlowchart (GAFc.altFcToFc fc) GCB.binCfgSCP

encodeJSON :: Aeson.ToJSON a => a -> BL.ByteString
encodeJSON = DAEP.encodePretty' prettyCfg
  where prettyCfg = DAEP.defConfig { DAEP.confIndent = DAEP.Spaces 2 }

putJSONBytes :: MonadIO m => BL.ByteString -> m ()
putJSONBytes a = do
    liftIO $ BL.putStr $ a
    liftIO $ putStrLn ""

putJSON :: MonadIO m => Aeson.ToJSON a => a -> m ()
putJSON = putJSONBytes . encodeJSON

--------------------------------------------------------------------------------

rParseFile
    :: (MonadReader CfgBinaryProcessing m, MonadIO m, ShowErrorComponent e)
    => (ParsecT e BS.ByteString (Reader GCB.BinaryCfg) a)
    -> GCB.BinaryCfg -> m (Either String a)
rParseFile p cfg = do
    fp <- asks _cfgBinaryProcessingFilepath
    GCBU.runParserBinFile p fp cfg

rBinOutputBytes
    :: (MonadReader env m, HasCfgBinaryProcessing env, MonadIO m)
    => BL.ByteString -> m ()
rBinOutputBytes bs = do
    -- binProcCfg <- asks getCfgBinaryProcessing
    asks (_cfgBinaryProcessingOutFilepath . getCfgBinaryProcessing) >>= \case
      Nothing -> do
        asks (_cfgBinaryProcessingAllowBinaryOnStdout . getCfgBinaryProcessing) >>= \case
          True -> do
            liftIO $ BL.putStr bs
          False -> do
            liftIO $ putStrLn "warning: refusing to print binary to stdout"
            liftIO $ putStrLn "(write to a file with --write-file FILE, or use --print-binary flag to override)"
      Just outFp -> do
        liftIO $ BL.writeFile outFp bs

rBinDecodeJsonAndReserialize
    :: (MonadReader env m, HasCfgBinaryProcessing env, MonadIO m, Aeson.FromJSON a)
    => (a -> BS.ByteString) -> m BS.ByteString
rBinDecodeJsonAndReserialize serialize = do
    binProcCfg <- asks getCfgBinaryProcessing
    let fp = _cfgBinaryProcessingFilepath binProcCfg
    bytes <- liftIO $ BL.readFile fp
    case Aeson.eitherDecode bytes of
      Left err      -> liftIO (putStrLn $ "JSON decoding error: " <> err) >>= error "fuck"
      Right decoded -> return $ serialize decoded
