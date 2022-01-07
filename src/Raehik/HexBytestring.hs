{- | Bytestrings as human-readable hex strings (e.g. @00 12 AB FF@).

Includes a 'BS.ByteString' newtype wrapper. Particularly useful for Aeson and
cases where you want to indicate human-readability in the type level.
-}

module Raehik.HexBytestring
  ( HexBytestring(..)
  , parseHexBytestring
  , prettyHexBytestring
  , prettyHexBytestringCompact
  ) where

import           GHC.Generics
import           Text.Megaparsec            hiding ( parse )
import qualified Text.Megaparsec.Char       as MC
import qualified Data.ByteString            as BS
import qualified Data.Char                  as Char
import           Data.Word
import qualified Data.Text                  as Text
import           Data.Text                  ( Text )
import           Data.List                  as List

newtype HexBytestring = HexBytestring { unHexBytestring :: BS.ByteString }
    deriving (Eq, Generic)

instance Show HexBytestring where
    show = Text.unpack . prettyHexBytestring . unHexBytestring

-- | A hex bytestring looks like this: @00 01 89 8a   FEff@. You can mix and
-- match capitalization and spacing, but I prefer to space each byte, full caps.
parseHexBytestring :: (MonadParsec e s m, Token s ~ Char) => m BS.ByteString
parseHexBytestring = BS.pack <$> parseHexByte `sepBy` MC.hspace

-- | Parse a byte formatted as two hex digits e.g. EF. You _must_ provide both
-- nibbles e.g. @0F@, not @F@. They cannot be spaced e.g. @E F@ is invalid.
--
-- Returns a value 0-255, so can fit in any Num type that can store that.
parseHexByte :: (MonadParsec e s m, Token s ~ Char, Num a) => m a
parseHexByte = do
    c1 <- MC.hexDigitChar
    c2 <- MC.hexDigitChar
    return $ 0x10 * fromIntegral (Char.digitToInt c1) + fromIntegral (Char.digitToInt c2)

-- | Pretty print to default format @00 12 AB FF@: space between each byte, all
--   caps.
--
-- This format I consider most human readable. I prefer caps to draw attention
-- to this being data instead of text (you don't see that many capital letters
-- packed together in prose).
prettyHexBytestring :: BS.ByteString -> Text
prettyHexBytestring =
      Text.concat
    . List.intersperse (Text.singleton ' ')
    . fmap (f . prettyHexByte Char.toUpper)
    . BS.unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2

prettyHexByte :: (Char -> Char) -> Word8 -> (Char, Char)
prettyHexByte f w = (prettyNibble h, prettyNibble l)
  where
    (h,l) = fromIntegral w `divMod` 0x10
    prettyNibble = f . Char.intToDigit -- Char.intToDigit returns lower case

-- | Pretty print to "compact" format @0012abff@ (often output by hashers).
prettyHexBytestringCompact :: BS.ByteString -> Text
prettyHexBytestringCompact =
    Text.concat . fmap (f . prettyHexByte id) . BS.unpack
  where
    f :: (Char, Char) -> Text
    f (c1, c2) = Text.cons c1 $ Text.singleton c2
