module GTVM.Common.Binary.Parse where
{-
  (
  -- * Unsigned integers
    pW8
  , pW16
  , pW32
  , pW64

  -- * Bytestrings
  , pBS
  , pBSTextFixed
  , pBS'
  , pCString
  , pPascal

  -- * Other combinators
  , pCount
  , pNullPadTo

  -- * Utilities
  , parseBin
  ) where
-}

import GTVM.Common.Binary
import Text.Megaparsec
import Text.Megaparsec.Byte.Binary
import Data.ByteString qualified as BS
import Data.Word
import Control.Monad.Reader
import Data.Either.Combinators ( mapLeft )

-- | Parse any byte.
pW8 :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word8
pW8 = word8

-- | Parse any 'Word16' (2-byte unsigned int) with given endianness.
pW16
    :: (MonadParsec e s m, BinaryChunk (Tokens s), MonadReader BinaryCfg m)
    => m Word16
pW16 = binCfgCaseEndianness word16le word16be

-- | Parse any 'Word32' (4-byte unsigned int) with given endianness.
pW32
    :: (MonadParsec e s m, BinaryChunk (Tokens s), MonadReader BinaryCfg m)
    => m Word32
pW32 = binCfgCaseEndianness word32le word32be

-- | Parse any 'Word64' (8-byte unsigned int) with given endianness.
pW64
    :: (MonadParsec e s m, BinaryChunk (Tokens s), MonadReader BinaryCfg m)
    => m Word64
pW64 = binCfgCaseEndianness word64le word64be

-- | Parse a fixed-length bytestring.
pBSFixed :: (MonadParsec e s m, Integral a) => a -> m (Tokens s)
pBSFixed len = takeP (Just (show len' <> "-byte bytestring")) len'
  where len' = fromIntegral len

-- | Parse a null-terminated bytestring (a C string), consuming the null.
pBSCString :: (MonadParsec e s m, Token s ~ Word8) => m (Tokens s)
pBSCString =
    takeWhileP (Just "non-null byte") (/= 0x00) <* pNullByte <?> "null-terminated bytestring"

-- | Parse a length-prefixed bytestring (a Pascal string).
--
-- The caller must provide the length prefix parser.
pBSPascal
    :: (MonadParsec e s m, Integral a)
    => m a
    -> m (Tokens s)
pBSPascal p = p >>= pBSFixed

-- | Parse the null byte.
pNullByte :: (MonadParsec e s m, Token s ~ Word8) => m Word8
pNullByte = single 0x00 <?> "null byte"

-- | Parse a null bytestring of set size.
pNullString :: (MonadParsec e s m, Token s ~ Word8) => Int -> m [Word8]
pNullString i = count i pNullByte <?> (show i <> "-byte null bytestring")

-- | Parse some integer, then apply a parser that many times.
pCount :: (MonadParsec e s m, Integral a) => m a -> m b -> m [b]
pCount pc pp  = pc >>= \i -> count (fromIntegral i) pp

-- | Run a parser, then parse a string of nulls until it reaches a set size.
--
-- Maybe a generic solution?
--
-- If the parser parses more than the requested size, it's a parse error.
pNullPadTo :: (MonadParsec e s m, Token s ~ Word8) => m a -> Int -> m a
pNullPadTo p i = do
    offsetStart <- stateOffset <$> getParserState
    parseResult <- p
    offsetEnd <- stateOffset <$> getParserState
    let parseLen = offsetEnd - offsetStart
        reqNulls = i - parseLen
    if   reqNulls < 0
    then error $ "TODO: send custom error: overlong block: " <> show parseLen <> " > " <> show i
    else pNullString reqNulls *> return parseResult

pCountNullPad :: (MonadParsec e s m, Token s ~ Word8, Integral n) => m n -> m a -> Int -> m [a]
pCountNullPad pc pp n = do
    i <- pc
    offsetStart <- stateOffset <$> getParserState
    parseResult <- count (fromIntegral i) pp
    offsetEnd <- stateOffset <$> getParserState
    let parseLen = offsetEnd - offsetStart
        reqNulls = n - parseLen
    if   reqNulls < 0
    then error $ "TODO: send custom error: overlong block: " <> show parseLen <> " > " <> show n
    else pNullString reqNulls *> return parseResult

-- | Parse a fixed-length bytestring, truncate to the first null and assert that
--   all following bytes are null.
--
-- TODO need MonoTraversable to remain polymorphic here. meh. Neater, better
-- than pNullPadTo, but lose genericity.
pBSFixedPadded :: (MonadParsec e BS.ByteString m, Integral a) => a -> m BS.ByteString
pBSFixedPadded len = do
    bs <- pBSFixed len
    case truncateBS bs of
      Just bs' -> return bs'
      Nothing  -> error $ "TODO: Megaparsec custom error: fixed padded bytestring had non-nulls following first null"

truncateBS :: BS.ByteString -> Maybe BS.ByteString
truncateBS bs =
    let (truncated, remainder) = BS.span (/= 0x00) bs
     in if   BS.all (== 0x00) remainder
        then Just truncated
        else Nothing

--------------------------------------------------------------------------------

parseBin
    :: (ShowErrorComponent e, VisualStream s, TraversableStream s)
    => (ReaderT BinaryCfg (Parsec e s)) a
    -> BinaryCfg -> FilePath -> s -> Either String a
parseBin p opts fp bs = mapLeft errorBundlePretty $ parse (runReaderT p opts) fp bs
