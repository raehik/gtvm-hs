{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module GTVM.SCP.Parse where

import           GTVM.SCP
import           Text.Megaparsec
import           Text.Megaparsec.Byte.Binary
import qualified Data.ByteString as BS
import           Data.Void
import           Data.Word
import           Data.List.NonEmpty ( NonEmpty(..) )
import           Control.Monad.Reader
import           Data.Either.Combinators ( mapLeft )

parseSCPFile :: MonadIO m => FilePath -> m (Either String [SCPSegment])
parseSCPFile fp = do
    bs <- liftIO $ BS.readFile fp
    return $ parseSCPBytes' fp scpOptsDef bs

parseSCPBytes :: Bytes -> Either String [SCPSegment]
parseSCPBytes = parseSCPBytes' "" scpOptsDef

parseSCPBytes' :: String -> SCPOpts -> Bytes -> Either String [SCPSegment]
parseSCPBytes' fp opts bs =
    mapLeft errorBundlePretty $ parse (runReaderT (many pSCPSeg) opts) fp bs

pSCPSeg :: (MonadParsec Void Bytes m, MonadReader SCPOpts m) => m SCPSegment
pSCPSeg = do
    byte <- pW8
    case byte of
      0x00 -> return SCPSeg00
      0x01 -> SCPSeg01BG <$> pBytestring <*> pW8 <*> pW8
      0x02 -> SCPSeg02SFX <$> pBytestring <*> pW8
      0x03 -> SCPSeg03 <$> pW8 <*> pBytestring <*> pW8
      0x04 -> SCPSeg04 <$> pW8 <*> pW8
      0x05 -> SCPSeg05Textbox <$> pW8 <*> pW32 <*> pBytestring <*> pBytestring <*> pW32
      -- no 0x06
      0x07 -> SCPSeg07SCP <$> pBytestring
      0x08 -> return SCPSeg08
      0x09 -> SCPSeg09 <$> pW8 <*> pCount pBSW32
      0x0A -> SCPSeg0A <$> pW8 <*> pW8 <*> pW32 <*> pW32 <*> pW32
      0x0B -> SCPSeg0B <$> pW8 <*> pW8
      0x0C -> SCPSeg0CFlag <$> pW8 <*> pW8
      0x0D -> SCPSeg0D <$> pW8
      0x0E -> SCPSeg0E <$> pW8
      0x0F -> return SCPSeg0F
      0x10 -> SCPSeg10 <$> pW8 <*> pW8 <*> pW8
      0x11 -> SCPSeg11EventCG <$> pBytestring
      0x12 -> return SCPSeg12
      0x13 -> SCPSeg13 <$> pW8 <*> pBytestring <*> pW8 <*> pW32
      0x14 -> SCPSeg14 <$> pW8
      0x15 -> return SCPSeg15
      0x16 -> return SCPSeg16Wadai
      0x17 -> SCPSeg17 <$> pW8 <*> pW8
      0x18 -> SCPSeg18 <$> pW8 <*> pW8
      0x19 -> SCPSeg19 <$> pW8 <*> pW8
      -- no 0x1A
      -- no 0x1B
      -- no 0x1C
      0x1D -> return SCPSeg1D
      0x1E -> SCPSeg1E <$> pW8
      0x1F -> return SCPSeg1FDelay
      0x20 -> SCPSeg20 <$> pW8
      0x21 -> return SCPSeg21
      0x22 -> SCPSeg22 <$> pBytestring <*> pCount pBSW32
      0x23 -> return SCPSeg23SFX
      0x24 -> SCPSeg24 <$> pBytestring
      0x25 -> return SCPSeg25
      0x26 -> return SCPSeg26
      0x27 -> SCPSeg27 <$> pBytestring <*> pW8 <*> pW8
      0x28 -> SCPSeg28 <$> pBytestring <*> pW8 <*> pW8
      0x29 -> SCPSeg29 <$> pBytestring <*> pW8 <*> pW8
      0x2A -> SCPSeg2A <$> pW8 <*> pW8
      0x2B -> SCPSeg2B <$> pW8 <*> pW8 <*> pW8 <*> pBytestring <*> pW8
      0x2C -> return SCPSeg2CMap
      0x2D -> SCPSeg2D <$> pBytestring <*> pW8 <*> pW8
      0x2E -> SCPSeg2E <$> pW8 <*> pW8 <*> pW32 <*> pW32
      0x2F -> SCPSeg2F <$> pW8
      0x30 -> SCPSeg30 <$> pW8
      0x31 -> return SCPSeg31
      0x32 -> SCPSeg32 <$> pW32 <*> pW8
      0x33 -> SCPSeg33 <$> pW8 <*> pW8
      0x34 -> SCPSeg34 <$> pW32
      0x35 -> SCPSeg35 <$> pBytestring
      0x36 -> SCPSeg36 <$> pW8 <*> pW32
      0x37 -> SCPSeg37 <$> pW8 <*> pW32
      0x38 -> SCPSeg38 <$> pW8 <*> pW32
      0x39 -> return SCPSeg39
      0x3A -> SCPSeg3A <$> pW8 <*> pW8
      0x3B -> SCPSeg3B <$> pW8
      0x3C -> SCPSeg3C <$> pW8 <*> pW8
      0x3D -> return SCPSeg3D
      0x3E -> SCPSeg3E <$> pW8 <*> pW8
      0x3F -> SCPSeg3F <$> pW8 <*> pW32 <*> pW32
      0x40 -> SCPSeg40 <$> pW8
      0x41 -> SCPSeg41 <$> pW8 <*> pW32 <*> pW32
      0x42 -> SCPSeg42 <$> pW8
      0x43 -> SCPSeg43SFX <$> pBytestring
      0x44 -> SCPSeg44SFX <$> pBytestring
      0x45 -> SCPSeg45SFX <$> pBytestring <*> pW8
      0x46 -> SCPSeg46 <$> pW8 <*> pW8
      0x47 -> SCPSeg47 <$> pW8 <*> pW8
      0x48 -> return SCPSeg48
      0x49 -> return SCPSeg49
      0x4A -> return SCPSeg4A
      0x4B -> return SCPSeg4B
      0x4C -> SCPSeg4C <$> pBytestring
      0x4D -> return SCPSeg4D
      0x4E -> return SCPSeg4E
      0x4F -> SCPSeg4F <$> pBytestring <*> pW8 <*> pW8
      0x50 -> SCPSeg50 <$> pBytestring <*> pW8 <*> pW8
      0x51 -> SCPSeg51 <$> pBytestring <*> pW8 <*> pW8
      0x52 -> SCPSeg52 <$> pBytestring
      0x53 -> SCPSeg53 <$> pBytestring <*> pW8 <*> pW8
      0x54 -> SCPSeg54 <$> pW8 <*> pW8
      0x55 -> SCPSeg55 <$> pW8 <*> pW8
      0x56 -> SCPSeg56 <$> pW8 <*> pW8
      0x57 -> SCPSeg57 <$> pW8 <*> pW8
      0x58 -> SCPSeg58 <$> pW8 <*> pW8
      0x59 -> SCPSeg59 <$> pW8
      0x5A -> SCPSeg5A <$> pW32 <*> pW8
      0x5B -> SCPSeg5B <$> pW32
      0x5C -> SCPSeg5C <$> pW8
      0x5D -> SCPSeg5D <$> pW32
      0x5E -> SCPSeg5E <$> pW32
      0x5F -> SCPSeg5F <$> pW32
      0x60 -> SCPSeg60 <$> pW32
      0x61 -> SCPSeg61 <$> pW8 <*> pW8
      0x62 -> SCPSeg62 <$> pW8
      0x63 -> return SCPSeg63
      0x64 -> return SCPSeg64
      0x65 -> return SCPSeg65Trophy
      0x66 -> return SCPSeg66
      0x67 -> return SCPSeg67
      0x68 -> return SCPSeg68
      0x69 -> return SCPSeg69
      0x6A -> SCPSeg6A <$> pW8
      0x6B -> SCPSeg6B <$> pW8
      0x6C -> SCPSeg6CWipe <$> pW8 <*> pW32 <*> pW32 <*> pW32
      0x6D -> SCPSeg6DWipe <$> pW8 <*> pW32 <*> pW32 <*> pW32
      0x6E -> return SCPSeg6E
      0x6F -> return SCPSeg6F
      0x70 -> SCPSeg70 <$> pW8 <*> pW8
      0x71 -> SCPSeg71 <$> pW8 <*> pW8
      0x72 -> return SCPSeg72
      0x73 -> SCPSeg73Kyoro <$> pW8 <*> pW32
      0x74 -> return SCPSeg74
      0x75 -> SCPSeg75 <$> pW8
      0x76 -> return SCPSeg76
      0x77 -> SCPSeg77SCP <$> pW8
      _    -> unexpected (Tokens (byte :| []))

-- | Parse a single byte, then apply a parser that many times.
pCount :: (MonadParsec e s m, Token s ~ Word8) => m a -> m [a]
pCount p = pW8 >>= \i -> count (fromIntegral i) p

-- | Parse a bytestring, followed by a 'Word32'.
pBSW32 :: (MonadParsec e Bytes m, MonadReader SCPOpts m) => m (Bytes, Word32)
pBSW32 = (,) <$> pBytestring <*> pW32

-- | Parse a bytestring.
--
-- The type of bytestring to parse is selected using provided context.
pBytestring :: (MonadParsec e Bytes m, MonadReader SCPOpts m) => m Bytes
pBytestring = reader scpOptStringType >>= \case
  StrTyCString      -> pCString
  StrTyLengthPrefix -> pPascal

-- | Parse a null-terminated bytestring (a C string), consuming the null.
pCString :: (MonadParsec e Bytes m) => m Bytes
pCString =
    takeWhileP (Just "non-null byte") (/= 0x00) <* pNullByte <?> "null-terminated bytestring"

-- | Parse the null byte.
pNullByte :: (MonadParsec e s m, Token s ~ Word8) => m Word8
pNullByte = single 0x00 <?> "null byte"

-- | Parse a length-prefixed bytestring (a Pascal string).
pPascal :: (MonadParsec e BS.ByteString m) => m BS.ByteString
pPascal = do
    len <- pW8
    let lbl = show len <> "-byte bytestring"
    takeP (Just lbl) (fromIntegral len)

-- | Parse any byte.
pW8 :: (MonadParsec e s m, Token s ~ Word8) => m Word8
pW8 = anyW8

-- | Parse any 'Word32'.
--
-- Endianness to parse is selected using provided context.
pW32 :: (MonadParsec e Bytes m, MonadReader SCPOpts m) => m Word32
pW32 = caseEndianness anyW32LE anyW32BE

--------------------------------------------------------------------------------

-- | Helper for switching functions based on context endianness.
caseEndianness :: MonadReader SCPOpts m => m a -> m a -> m a
caseEndianness fLE fBE = reader scpOptEndianness >>= \case
  LittleEndian -> fLE
  BigEndian    -> fBE

--------------------------------------------------------------------------------

pWrap
    :: (MonadParsec e s m, Token s ~ Word8)
    => m a -> m (Either Word8 a)
pWrap p = (Right <$> try p) <|> (Left <$> anySingle)
