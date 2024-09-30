module Common.Util where

import Common.Config
import Control.Monad.IO.Class
import Data.ByteString qualified as BS
import System.Exit
import Prettyprinter
import Data.Either.Validation
import Strongweak

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

badExit :: (MonadIO m, Show s) => String -> s -> m a
badExit errStr s = do
    liftIO $ putStrLn $ "error: while " <> errStr <> ": " <> show s
    liftIO $ exitWith $ ExitFailure 1

liftErr :: MonadIO m => (e -> String) -> Either e a -> m a
liftErr f = \case Left  e -> exit (f e)
                  Right a -> return a

exit :: MonadIO m => String -> m a
exit s = do liftIO $ putStrLn $ "error: "<>s
            liftIO $ exitWith $ ExitFailure 2

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

liftStrengthen :: (MonadIO m, Strengthen a) => Weak a -> m a
liftStrengthen wa =
    case strengthen wa of
      Right sa  -> pure sa
      Left  err -> liftIO $ do
        print err -- TODO no strongweak prettifier yet. oops
        exitWith $ ExitFailure 3
