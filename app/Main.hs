{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Config
import           CLI

import qualified GTVM.Assorted.Flowchart as GAFc
import qualified GTVM.Common.Binary.Util as GCBU
import qualified GTVM.Common.Binary      as GCB

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as DAEP
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString          as BS

import           Control.Monad.IO.Class
import           Control.Monad.Reader

import           System.IO ( stderr, hPutStrLn )

main :: IO ()
main = parseCLIOpts >>= runCmd

runCmd :: ToolGroup -> IO ()
runCmd = \case
  TGFlowchart cfg -> runReaderT runCmdFlowchart cfg
  TGSCP (TGSCPCfg binProcCfg) -> runReaderT runCmdSCP binProcCfg

runCmdSCP :: (MonadReader CfgBinaryProcessing m, MonadIO m) => m ()
runCmdSCP = do
    liftIO $ putStrLn $ "TODO. Printing config for now."
    ask >>= liftIO . print

runCmdFlowchart :: (MonadReader TGFlowchartCfg m, MonadIO m) => m ()
runCmdFlowchart = do
    binProcCfg <- asks _tgFlowchartCfgBinaryProcessing
    case _cfgBinaryProcessingDirection binProcCfg of
      ActionDirectionDecode -> do
        json <- runCmdFlowchartDecode
        putJSONBytes json
      ActionDirectionEncode -> do
        json <- runCmdFlowchartEncode
        case _cfgBinaryProcessingOutFilepath binProcCfg of
          Nothing -> do
            case _cfgBinaryProcessingAllowBinaryOnStdout binProcCfg of
              True -> do
                liftIO $ BS.putStr json
              False -> do
                liftIO $ putStrLn "warning: refusing to print binary to stdout"
                liftIO $ putStrLn "(use --print-binary flag to override)"
          Just outFp -> do
            liftIO $ BS.writeFile outFp json

runCmdFlowchartDecode :: (MonadReader TGFlowchartCfg m, MonadIO m) => m BL.ByteString
runCmdFlowchartDecode = do
    binProcCfg <- asks _tgFlowchartCfgBinaryProcessing
    let fp = _cfgBinaryProcessingFilepath binProcCfg
    lexed <- GCBU.runParserBinFile GAFc.pFlowchart fp GCB.binCfgSCP
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
    binProcCfg <- asks _tgFlowchartCfgBinaryProcessing
    let fp = _cfgBinaryProcessingFilepath binProcCfg
    bytes <- liftIO $ BL.readFile fp
    case Aeson.eitherDecode bytes of
      Left err -> liftIO (putStrLn $ "JSON decoding error: " <> err) >>= error "fuck"
      Right flowchart -> do
        let serialized = GAFc.sFlowchart (GAFc.altFcToFc flowchart) GCB.binCfgSCP
        return serialized

encodeJSON :: Aeson.ToJSON a => a -> BL.ByteString
encodeJSON = DAEP.encodePretty' prettyCfg
  where prettyCfg = DAEP.defConfig { DAEP.confIndent = DAEP.Spaces 2 }

putJSONBytes :: MonadIO m => BL.ByteString -> m ()
putJSONBytes a = do
    liftIO $ BL.putStr $ a
    liftIO $ putStrLn ""

putJSON :: MonadIO m => Aeson.ToJSON a => a -> m ()
putJSON = putJSONBytes . encodeJSON
