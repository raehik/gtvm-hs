{-# LANGUAGE TypeFamilies #-}

module GTVM.SCP.Parse where

import           GTVM.SCP
import           Text.Megaparsec
import           Text.Megaparsec.Byte.Binary
import qualified Data.ByteString as BS
import           Data.Void
import           Data.Word
import           Data.List.NonEmpty ( NonEmpty(..) )

type Parser = Parsec Void BS.ByteString

pSCPSeg :: Parser SCPSegment
pSCPSeg = do
    byte <- anyW8
    case byte of
      0x05 -> pSCPSegTextbox
      0x10 -> pSCPSegUnk10
      0x01 -> pSCPSegBg
      0x08 -> return SCPSegUnk08
      0x34 -> pSCPSegUnk34
      0x02 -> pSCPSegSFX
      _    -> unexpected (Tokens (byte :| []))

pSCPSegTextbox :: Parser SCPSegment
pSCPSegTextbox = do
    unkHeaderByte <- anyW8
    speakerId     <- anyW32LE
    text          <- pNullTermByteString
    voicePath     <- pNullTermByteString
    unkCounter    <- anyW32LE
    return $ SCPSegTextbox unkHeaderByte speakerId text voicePath unkCounter

pSCPSegBg :: Parser SCPSegment
pSCPSegBg = do
    bgPath <- pNullTermByteString
    b1 <- anyW8
    b2 <- anyW8
    b3 <- anyW8
    return $ SCPSegBg bgPath b1 b2 b3

pSCPSegSFX :: Parser SCPSegment
pSCPSegSFX = do
    sfxPath <- pNullTermByteString
    byte <- anyW8
    return $ SCPSegSFX sfxPath byte

pSCPSegUnk34 :: Parser SCPSegment
pSCPSegUnk34 = do
    w32 <- anyW32LE
    return $ SCPSegUnk34 w32

pSCPSegUnk10 :: Parser SCPSegment
pSCPSegUnk10 = do
    b1 <- anyW8
    b2 <- anyW8
    b3 <- anyW8
    return $ SCPSegUnk10 b1 b2 b3

pWrap
    :: (MonadParsec e s m, Token s ~ Word8)
    => m a -> m (Either Word8 a)
pWrap p = (Right <$> try p) <|> (Left <$> anySingle)

pSCPSegs :: Parser [SCPSegment]
pSCPSegs = many pSCPSegTextbox

pNullTermByteString :: Parser BS.ByteString
pNullTermByteString =
    takeWhileP (Just "non-null byte") (/= 0x00) <* pNullByte <?> "null-term str"

pNullByte :: Parser Word8
pNullByte = single 0x00 <?> "null byte"
