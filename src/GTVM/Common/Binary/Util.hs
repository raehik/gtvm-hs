{-# LANGUAGE TypeFamilies #-}

module GTVM.Common.Binary.Util
  ( runParserBin
  , runParserBin'
  , runParserBinFile
  ) where

import GTVM.Common.Binary
import Text.Megaparsec
import Data.ByteString qualified as BS
import Control.Monad.Reader
import Data.Either.Combinators ( mapLeft )

type Bytes = BS.ByteString

runParserBin
    :: (ShowErrorComponent e, MonadReader BinaryCfg m)
    => ParsecT e Bytes m a -> Bytes -> m (Either String a)
runParserBin p = runParserBin' p ""

runParserBinFile
    :: (ShowErrorComponent e, MonadIO m)
    => ParsecT e Bytes (Reader BinaryCfg) a -> FilePath -> BinaryCfg -> m (Either String a)
runParserBinFile p fp cfg = do
    bs <- liftIO $ BS.readFile fp
    return $ flip runReader cfg (runParserBin p bs)

runParserBin'
    :: (ShowErrorComponent e, MonadReader BinaryCfg m)
    => ParsecT e Bytes m a -> String -> Bytes -> m (Either String a)
runParserBin' p fp bs = mapLeft errorBundlePretty <$> runParserT p fp bs
