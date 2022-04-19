module GTVM.Common.IO where

import Data.Aeson
import Data.Yaml qualified as Yaml
import Data.ByteString qualified as BS
import Control.Monad.IO.Class

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
