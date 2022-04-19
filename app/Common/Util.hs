module Common.Util where

import Common.Config
import Control.Monad.IO.Class
import Data.Aeson
import Data.Yaml qualified as Yaml
import Data.Yaml.Pretty qualified as YamlPretty
import Data.ByteString qualified as BS
import System.Exit

encodeYamlPretty :: ToJSON a => a -> BS.ByteString
encodeYamlPretty = YamlPretty.encodePretty yamlPrettyCfg
  where
    yamlPrettyCfg :: YamlPretty.Config
    yamlPrettyCfg = YamlPretty.setConfDropNull True YamlPretty.defConfig

readStreamBytes :: MonadIO m => Stream 'StreamIn s -> m BS.ByteString
readStreamBytes = \case
  StreamFile' sf -> readStreamFileBytes sf
  StreamStd      -> readStdinBytes

readStreamFileBytes :: MonadIO m => StreamFile 'StreamIn s -> m BS.ByteString
readStreamFileBytes = liftIO . BS.readFile . streamFilePath

readStdinBytes :: MonadIO m => m BS.ByteString
readStdinBytes = liftIO BS.getContents

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

writeStreamTextualBytes :: MonadIO m => Stream 'StreamOut s -> BS.ByteString -> m ()
writeStreamTextualBytes s bs =
    case s of
      StreamFile' sf -> liftIO $ BS.writeFile (streamFilePath sf) bs
      StreamStd      -> liftIO $ BS.putStr bs

writeStreamBin :: MonadIO m => PrintBin -> Stream 'StreamOut s -> BS.ByteString -> m ()
writeStreamBin pb s bs =
    case s of
      StreamFile' sf -> liftIO $ BS.writeFile (streamFilePath sf) bs
      StreamStd      ->
        case pb of
          PrintBin   -> liftIO $ BS.putStr bs
          NoPrintBin -> do
            liftIO $ putStrLn "warning: refusing to print binary to stdout"
            liftIO $ putStrLn "(write to a file with --out-file FILE, or use --print-binary flag to override)"

-- TODO: bad. decoding may fail, that's why we gotta do this.
badParseYAML :: forall a m. (MonadIO m, FromJSON a) => BS.ByteString -> m a
badParseYAML bs =
    case Yaml.decodeEither' bs of
      Left err      -> do
        liftIO $ putStrLn "error decoding YAML: "
        liftIO $ print err
        error "fucko"
        --liftIO $ Exit.exitWith (Exit.ExitFailure 1)
      Right decoded -> return decoded

badExit :: (MonadIO m, Show s) => String -> s -> m a
badExit errStr s = do
    liftIO $ putStrLn $ "error: while " <> errStr <> ": " <> show s
    liftIO $ exitWith $ ExitFailure 1

badParseStream
    :: MonadIO m
    => (FilePath -> BS.ByteString -> Either String a)
    -> Stream 'StreamIn s
    -> m a
badParseStream f = \case
  StreamFile' sf -> do
    bs <- readStreamFileBytes sf
    case f (streamFilePath sf) bs of
      Left  err -> badExit "parsing input" err
      Right out -> return out
  StreamStd     -> do
    bs <- readStdinBytes
    case f "<stdin>" bs of
      Left  err -> badExit "parsing input" err
      Right out -> return out
