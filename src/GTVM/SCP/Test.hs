{-# LANGUAGE FlexibleContexts #-}

module GTVM.SCP.Test where

import           GTVM.Common.Binary
import           GTVM.SCP.Parse
import           GTVM.SCP.Serialize
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import qualified Data.List as List

type Bytes = BS.ByteString

-- | Parse, then re-serialize.
reserialize :: MonadReader BinaryCfg m => Bytes -> m Bytes
reserialize bs = do
    binCfg <- ask
    let Right segs = parseSCPBytes' binCfg "" bs
    sSCP segs

-- | Parse, then re-serialize.
reserializeSCPDir :: MonadIO m => BinaryCfg -> FilePath -> m ()
reserializeSCPDir cfg dir = do
    files <- liftIO $ Dir.listDirectory dir
    let scpFiles = filter (List.isSuffixOf ".scp") files
    flip mapM_ scpFiles $ \fp -> do
        bs <- liftIO $ BS.readFile (dir <> "/" <> fp)
        let bs' = reserialize bs cfg
        if   bs /= bs'
        then liftIO $ putStrLn fp
        else return ()
