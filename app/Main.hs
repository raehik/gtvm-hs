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
  TGSCP (TGSCPCfg cfgBinJSON) -> runReaderT runCmdSCP cfgBinJSON

runCmdSCP :: (MonadReader CfgBinaryJSON m, MonadIO m) => m ()
runCmdSCP = do
    asks _cfgBinaryJSONDirection >>= \case
      ActionDirectionDecode -> runCmdScpDecode >>= rBinOutputBytes
      ActionDirectionEncode -> runCmdScpEncode >>= rBinOutputBytes . BL.fromStrict

runCmdScpDecode :: (MonadReader CfgBinaryJSON m, MonadIO m) => m BL.ByteString
runCmdScpDecode = do
    rParseFile GSP.pSCP GCB.binCfgSCP >>= \case
      Left err     -> liftIO (putStr err) >> error "fuck"
      Right parsed -> rEncodeJSON parsed

runCmdScpEncode :: (MonadReader CfgBinaryJSON m, MonadIO m) => m BS.ByteString
runCmdScpEncode = rBinDecodeJsonAndReserialize (flip GSS.sSCP GCB.binCfgSCP)

runCmdFlowchart :: (MonadReader TGFlowchartCfg m, MonadIO m) => m ()
runCmdFlowchart = do
    binProcCfg <- asks _tgFlowchartCfgBinaryJSON
    case _cfgBinaryJSONDirection binProcCfg of
      ActionDirectionDecode -> runCmdFlowchartDecode >>= rBinOutputBytes
      ActionDirectionEncode -> runCmdFlowchartEncode >>= rBinOutputBytes . BL.fromStrict

runCmdFlowchartDecode :: (MonadReader TGFlowchartCfg m, MonadIO m) => m BL.ByteString
runCmdFlowchartDecode = do
    binProcCfg <- asks _tgFlowchartCfgBinaryJSON
    lexed <- flip runReaderT binProcCfg $ rParseFile GAFc.pFlowchart GCB.binCfgSCP
    case lexed of
      Left err      -> liftIO (putStr err) >> error "fuck"
      Right lexed' -> do
        ty <- asks _tgFlowchartCfgType
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
    :: (MonadReader env m, HasCfgBinaryJSON env, MonadIO m, Aeson.ToJSON a)
    => a -> m BL.ByteString
rEncodeJSON a = do
    cfgBinJSON <- asks getCfgBinaryJSON
    if   _cfgBinaryJSONPrettify cfgBinJSON
    then return $ DAEP.encodePretty' prettyCfg a
    else return $ Aeson.encode a
  where prettyCfg = DAEP.defConfig { DAEP.confIndent = DAEP.Spaces 2 }

--------------------------------------------------------------------------------

rParseFile
    :: (MonadReader CfgBinaryJSON m, MonadIO m, ShowErrorComponent e)
    => (ParsecT e BS.ByteString (Reader GCB.BinaryCfg) a)
    -> GCB.BinaryCfg -> m (Either String a)
rParseFile p cfg = do
    fp <- asks _cfgBinaryJSONFilepath
    GCBU.runParserBinFile p fp cfg

rBinOutputBytes
    :: (MonadReader env m, HasCfgBinaryJSON env, MonadIO m)
    => BL.ByteString -> m ()
rBinOutputBytes bs = do
    cfgBinJSON <- asks getCfgBinaryJSON
    case _cfgBinaryJSONOutFilepath cfgBinJSON of
      Just outFp -> liftIO $ BL.writeFile outFp bs
      Nothing -> do
        if   _cfgBinaryJSONDirection cfgBinJSON == ActionDirectionDecode
        then liftIO $ BL.putStr bs >> putStrLn ""
        else
            case _cfgBinaryJSONAllowBinaryOnStdout cfgBinJSON of
              True -> liftIO $ BL.putStr bs
              False -> do
                liftIO $ putStrLn "warning: refusing to print binary to stdout"
                liftIO $ putStrLn "(write to a file with --write-file FILE, or use --print-binary flag to override)"

rBinDecodeJsonAndReserialize
    :: (MonadReader env m, HasCfgBinaryJSON env, MonadIO m, Aeson.FromJSON a)
    => (a -> BS.ByteString) -> m BS.ByteString
rBinDecodeJsonAndReserialize serialize = do
    cfgBinJSON <- asks getCfgBinaryJSON
    let fp = _cfgBinaryJSONFilepath cfgBinJSON
    bytes <- liftIO $ BL.readFile fp
    case Aeson.eitherDecode bytes of
      Left err      -> liftIO (putStrLn $ "JSON decoding error: " <> err) >>= error "fuck"
      Right decoded -> return $ serialize decoded
