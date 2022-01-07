module Util where

import           Config
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Yaml              as Yaml
import qualified Data.Yaml.Pretty       as YamlPretty
import qualified Data.ByteString        as BS
import           System.Exit

-- TODO: bad. decoding may fail, that's why we gotta do this.
forceParseYaml :: forall a m. (MonadIO m, FromJSON a) => BS.ByteString -> m a
forceParseYaml bs =
    case Yaml.decodeEither' bs of
      Left err      -> do
        liftIO $ putStrLn $ "error decoding YAML: "
        liftIO $ print err
        error "fucko"
        --liftIO $ Exit.exitWith (Exit.ExitFailure 1)
      Right decoded -> return decoded

encodeYamlPretty :: ToJSON a => a -> BS.ByteString
encodeYamlPretty = YamlPretty.encodePretty yamlPrettyCfg
  where
    yamlPrettyCfg :: YamlPretty.Config
    yamlPrettyCfg = YamlPretty.setConfDropNull True YamlPretty.defConfig

readStreamBytes :: MonadIO m => CStream -> m BS.ByteString
readStreamBytes = \case
  CStreamFile fp -> readFileBytes fp
  CStreamStd     -> readStdinBytes

readFileBytes :: MonadIO m => FilePath -> m BS.ByteString
readFileBytes = liftIO . BS.readFile

readStdinBytes :: MonadIO m => m BS.ByteString
readStdinBytes = liftIO BS.getContents

print' :: (MonadIO m, Show a) => a -> m ()
print' = liftIO . print

writeStreamTextualBytes :: MonadIO m => CStream -> BS.ByteString -> m ()
writeStreamTextualBytes s bs =
    case s of
      CStreamFile fp -> liftIO $ BS.writeFile fp bs
      CStreamStd     -> liftIO $ BS.putStr bs

naughtyExit :: (MonadIO m, Show s) => String -> s -> m a
naughtyExit errStr s = do
    liftIO $ putStrLn $ "error: while " <> errStr <> ": " <> show s
    liftIO $ exitWith $ ExitFailure 1
