{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module GTVM.Common.Parse
  ( pW8
  , pW32
  , pBytestring
  , pCString
  , pPascal
  , pNullByte
  , pCount
  ) where

import           GTVM.Common.Binary
import           Text.Megaparsec
import           Text.Megaparsec.Byte.Binary
import qualified Data.ByteString as BS
import           Data.Word
import           Control.Monad.Reader

type Bytes = BS.ByteString

-- | Parse any byte.
pW8 :: (MonadParsec e s m, Token s ~ Word8) => m Word8
pW8 = anyW8

-- | Parse any 'Word32' with given endianness.
pW32 :: (MonadParsec e Bytes m, MonadReader BinaryCfg m) => m Word32
pW32 = binCfgCaseEndianness anyW32LE anyW32BE

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
pPascal :: (MonadParsec e BS.ByteString m) => m BS.ByteString
pPascal = do
    len <- pW8
    let lbl = show len <> "-byte bytestring"
    takeP (Just lbl) (fromIntegral len)

-- | Parse the null byte.
pNullByte :: (MonadParsec e s m, Token s ~ Word8) => m Word8
pNullByte = single 0x00 <?> "null byte"

-- | Parse a single byte, then apply a parser that many times.
pCount :: (MonadParsec e s m, Token s ~ Word8) => m a -> m [a]
pCount p = pW8 >>= \i -> count (fromIntegral i) p
