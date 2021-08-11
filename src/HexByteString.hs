{-# LANGUAGE TypeFamilies #-}

module HexByteString
  ( HexByteString(..)
  , pHexByteString
  , pHexByteString'
  , prettyHexByteString
  , prettyHexByteString'
  ) where

import           Text.Megaparsec
import qualified Text.Megaparsec.Char       as MC
import qualified Text.Megaparsec.Byte       as MB
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.Char                  as Char
import           Data.Word
import qualified Data.Text                  as Text
import           Data.Text                  ( Text )
import           Data.List                  as List
import qualified Text.Ascii                 as Ascii

type Bytes = BS.ByteString

newtype HexByteString = HexByteString { unHexByteString :: BS.ByteString }
    deriving (Eq)

instance Show HexByteString where
    show = Text.unpack . prettyHexByteString . unHexByteString

-- | A hex bytestring looks like this: @00 01 89 8a   FEff@. You can mix and
-- match capitalization and spacing, but I prefer to space each byte, full caps.
pHexByteString :: (MonadParsec e s m, Token s ~ Char) => m Bytes
pHexByteString = BS.pack <$> pHexByte `sepBy` MC.hspace

-- | Parse a byte formatted as two hex digits e.g. EF. You _must_ provide both
-- nibbles e.g. @0F@, not @F@. They cannot be spaced e.g. @E F@ is invalid.
--
-- Returns a value 0-255, so can fit in any Num type that can store that.
pHexByte :: (MonadParsec e s m, Token s ~ Char, Num a) => m a
pHexByte = do
    c1 <- MC.hexDigitChar
    c2 <- MC.hexDigitChar
    return $ 0x10 * fromIntegral (Char.digitToInt c1) + fromIntegral (Char.digitToInt c2)

prettyHexByteString :: Bytes -> Text
prettyHexByteString =
    Text.concat . List.intersperse (Text.singleton ' ') . fmap (f . prettyByte) . BS.unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2

prettyHexByteString' :: Bytes -> BB.Builder
prettyHexByteString' =
    mconcat . List.intersperse (BB.word8 0x20) . fmap (f . prettyByte) . BS.unpack
  where
    f :: (Char, Char) -> BB.Builder
    f (c1, c2) = BB.char7 c1 <> BB.char7 c2

prettyByte :: Word8 -> (Char, Char)
prettyByte w = (prettyNibble h, prettyNibble l)
  where
    (h,l) = fromIntegral w `divMod` 0x10
    prettyNibble = Char.toUpper . Char.intToDigit

pHexByteString' :: (MonadParsec e s m, Token s ~ Word8) => m Bytes
pHexByteString' = BS.pack <$> pHexByte' `sepBy` MB.hspace

pHexByte' :: (MonadParsec e s m, Token s ~ Word8, Num a) => m a
pHexByte' = do
    c1 <- MB.hexDigitChar
    c2 <- MB.hexDigitChar
    return $ 0x10 * Ascii.unsafeFromHexDigit8 c1 + Ascii.unsafeFromHexDigit8 c2
