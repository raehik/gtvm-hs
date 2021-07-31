{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module GTVM.Common.Binary.Parse
  ( pW8
  , pW32
  , pW64
  , pBytestring
  , pCString
  , pPascal
  , pBytestringFixed
  , pCStringPad
  , pNullByte
  , pCount
  , pNullPadTo
  , parseBin
  ) where

import           GTVM.Common.Binary
import           Text.Megaparsec
import           Text.Megaparsec.Byte.Binary
import qualified Data.ByteString as BS
import           Data.Word
import           Control.Monad.Reader
import           Data.Either.Combinators ( mapLeft )

type Bytes = BS.ByteString

-- | Parse any byte.
pW8 :: (MonadParsec e s m, Token s ~ Word8) => m Word8
pW8 = anyW8

-- | Parse any 'Word32' (4-byte unsigned int) with given endianness.
pW32 :: (MonadParsec e Bytes m, MonadReader BinaryCfg m) => m Word32
pW32 = binCfgCaseEndianness anyW32LE anyW32BE

-- | Parse any 'Word64' (8-byte unsigned int) with given endianness.
pW64 :: (MonadParsec e Bytes m, MonadReader BinaryCfg m) => m Word64
pW64 = binCfgCaseEndianness anyW64LE anyW64BE

-- | Parse a bytestring.
--
-- The type of bytestring to parse is selected using provided context.
pBytestring :: (MonadParsec e Bytes m, MonadReader BinaryCfg m) => m Bytes
pBytestring = binCfgCaseStringType pCString pPascal

-- | Parse a null-terminated bytestring (a C string), consuming the null.
pCString :: (MonadParsec e Bytes m) => m Bytes
pCString =
    takeWhileP (Just "non-null byte") (/= 0x00) <* pNullByte <?> "null-terminated bytestring"

-- | Parse a length-prefixed bytestring (a Pascal string).
--
-- Uses a single byte for length, so only lengths of 0->255 are permitted.
pPascal :: (MonadParsec e BS.ByteString m) => m BS.ByteString
pPascal = pW8 >>= \len -> pBytestringFixed (fromIntegral len)

-- | Parse a fixed-length bytestring (no null terminator).
pBytestringFixed :: (MonadParsec e Bytes m) => Int -> m Bytes
pBytestringFixed len = takeP (Just (show len <> "-byte bytestring")) len

-- | Parse a "fixed-length" null-terminated bytestring, consuming nulls after
--   the null terminator to reach a given size.
pCStringPad :: (MonadParsec e Bytes m) => Int -> m Bytes
pCStringPad len = pNullPadTo pCString len <?> "null-padded fixed-size null-terminated bytestring"

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
-- If the parser parses more than the requested size, it's a parse error.
pNullPadTo :: (MonadParsec e Bytes m) => m a -> Int -> m a
pNullPadTo p i = do
    offsetStart <- stateOffset <$> getParserState
    parseResult <- p
    offsetEnd <- stateOffset <$> getParserState
    let parseLen = offsetEnd - offsetStart
        reqNulls = i - parseLen
    if   reqNulls < 0
    then error $ "TODO: send custom error: overlong block: " <> show parseLen <> " > " <> show i
    else pNullString reqNulls *> return parseResult

parseBin
    :: (ShowErrorComponent e)
    => (ReaderT BinaryCfg (Parsec e Bytes)) a
    -> String -> BinaryCfg -> Bytes -> Either String a
parseBin p fp opts bs = mapLeft errorBundlePretty $ parse (runReaderT p opts) fp bs
