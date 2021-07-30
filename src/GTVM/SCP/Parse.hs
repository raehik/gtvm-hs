{-# LANGUAGE TypeFamilies #-}

module GTVM.SCP.Parse where

import           GTVM.SCP
import           Text.Megaparsec
import           Text.Megaparsec.Byte.Binary
import qualified Data.ByteString as BS
import           Data.Void
import           Data.Word
import           Data.List.NonEmpty ( NonEmpty(..) )

type Parser = Parsec Void Bytes

pSCPSeg :: Parser SCPSegment
pSCPSeg = do
    byte <- anyW8
    case byte of
      0x00 -> return SCPSeg00
      0x01 -> SCPSeg01BG <$> pCString <*> anyW8 <*> anyW8
      0x02 -> SCPSeg02SFX <$> pCString <*> anyW8
      0x03 -> SCPSeg03 <$> anyW8 <*> pCString <*> anyW8
      0x04 -> SCPSeg04 <$> anyW8 <*> anyW8
      0x05 -> pSCPSeg05Textbox
      -- can't see an 0x06 case in code
      0x07 -> SCPSeg07SCP <$> pCString
      0x08 -> return SCPSeg08
      0x09 -> pSCPSeg09
      0x0A -> pSCPSeg0A
      0x0B -> SCPSeg0B <$> anyW8 <*> anyW8
      0x0C -> SCPSeg0CFlag <$> anyW8 <*> anyW8
      0x0D -> SCPSeg0D <$> anyW8
      0x0E -> SCPSeg0E <$> anyW8
      0x0F -> return SCPSeg0F
      0x10 -> SCPSeg10 <$> anyW8 <*> anyW8 <*> anyW8
      0x11 -> SCPSeg11EventCG <$> pCString
      0x12 -> return SCPSeg12
      0x13 -> SCPSeg13 <$> anyW8 <*> pCString <*> anyW8 <*> anyW32LE
      0x14 -> SCPSeg14 <$> anyW8
      0x15 -> return SCPSeg15
      0x16 -> return SCPSeg16Wadai
      0x17 -> SCPSeg17 <$> anyW8 <*> anyW8
      0x18 -> SCPSeg18 <$> anyW8 <*> anyW8
      0x19 -> SCPSeg19 <$> anyW8 <*> anyW8
      0x1D -> return SCPSeg1D
      0x1E -> SCPSeg1E <$> anyW8
      0x1F -> return SCPSeg1FDelay
      0x20 -> SCPSeg20 <$> anyW8
      0x21 -> return SCPSeg21
      -- TODO 0x22 -> SCPSeg22 <$>1
      0x30 -> SCPSeg30 <$> anyW8
      0x31 -> return SCPSeg31
      0x32 -> SCPSeg32 <$> anyW32LE <*> anyW8
      0x34 -> SCPSeg34 <$> anyW32LE
      0x3A -> SCPSeg3A <$> anyW8 <*> anyW8
      0x3C -> SCPSeg3C <$> anyW8 <*> anyW8
      0x41 -> SCPSeg41 <$> anyW8 <*> anyW32LE <*> anyW32LE
      0x43 -> SCPSeg43SFX <$> pCString
      0x44 -> SCPSeg44SFX <$> pCString
      0x45 -> SCPSeg45SFX <$> pCString <*> anyW8
      0x46 -> SCPSeg46 <$> anyW8 <*> anyW8
      0x54 -> SCPSeg54 <$> anyW8 <*> anyW8
      0x55 -> SCPSeg55 <$> anyW8 <*> anyW8
      0x56 -> SCPSeg56 <$> anyW8 <*> anyW8
      0x5C -> SCPSeg5C <$> anyW8
      0x5D -> SCPSeg5D <$> anyW32LE
      0x5E -> SCPSeg5E <$> anyW32LE
      0x5F -> SCPSeg5F <$> anyW32LE
      0x60 -> SCPSeg60 <$> anyW32LE
      0x68 -> return SCPSeg68
      0x6C -> SCPSeg6CWipe <$> anyW8 <*> anyW32LE <*> anyW32LE <*> anyW32LE
      0x6D -> SCPSeg6DWipe <$> anyW8 <*> anyW32LE <*> anyW32LE <*> anyW32LE
      0x6E -> return SCPSeg6E
      0x6F -> return SCPSeg6F
      0x70 -> SCPSeg70 <$> anyW8 <*> anyW8
      0x71 -> SCPSeg71 <$> anyW8 <*> anyW8
      0x72 -> return SCPSeg72
      0x73 -> SCPSeg73Kyoro <$> anyW8 <*> anyW32LE
      0x74 -> return SCPSeg74
      0x75 -> SCPSeg75 <$> anyW8
      0x77 -> SCPSeg77SCP <$> anyW8
      _    -> unexpected (Tokens (byte :| []))

pSCPSeg05Textbox :: Parser SCPSegment
pSCPSeg05Textbox = do
    bUnk          <- anyW8
    speakerId     <- anyW32LE
    text          <- pCString
    voicePath     <- pCString
    counterUnk    <- anyW32LE
    return $ SCPSeg05Textbox bUnk speakerId text voicePath counterUnk

pSCPSeg09 :: Parser SCPSegment
pSCPSeg09 = do
    bUnk    <- anyW8
    counter <- anyW8
    values  <- count (fromIntegral counter) ((,) <$> pCString <*> anyW32LE)
    return $ SCPSeg09 bUnk values

-- TODO: Nasty one. Looks like the Word32s are used to calculate something.
pSCPSeg0A :: Parser SCPSegment
pSCPSeg0A = SCPSeg0A <$> anyW8 <*> anyW8 <*> anyW32LE <*> anyW32LE <*> anyW32LE

pWrap
    :: (MonadParsec e s m, Token s ~ Word8)
    => m a -> m (Either Word8 a)
pWrap p = (Right <$> try p) <|> (Left <$> anySingle)

-- | Parse a null-terminated bytestring (a C-style string), consuming the null.
pCString :: Parser Bytes
pCString =
    takeWhileP (Just "non-null byte") (/= 0x00) <* pNullByte <?> "null-terminated bytestring"

pNullByte :: Parser Word8
pNullByte = single 0x00 <?> "null byte"

--------------------------------------------------------------------------------

parseSCPFile :: FilePath -> IO ()
parseSCPFile fp = do
    scpBytes <- BS.readFile fp
    parseTest (many pSCPSeg) scpBytes
