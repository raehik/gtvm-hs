{-# LANGUAGE FlexibleContexts #-}

module GTVM.SCP.Serialize where

import           GTVM.SCP
import qualified Data.ByteString            as BS
import           Data.Word
import           Control.Monad.Reader
import           Control.Applicative
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
--import           ByteString.StrictBuilder

type Builder = BB.Builder

serializeSCP :: MonadReader SCPOpts m => SCPSegment -> m Bytes
serializeSCP x = serializeBuilder =<< case x of
  SCPSeg00   -> sW8 0x00
  SCPSeg01BG bs1 b1  b2 -> mmconcat [sW8 0x01, sBS bs1, sW8 b1, sW8 b2]
  SCPSeg02SFX    bs1 b1 -> mmconcat [sW8 0x02, sBS bs1, sW8 b1]
  SCPSeg03   b1  bs1 b2 -> mmconcat [sW8 0x03, sW8 b1, sBS bs1, sW8 b2]
  SCPSeg04       b1  b2 -> mmconcat [sW8 0x04, sW8 b1, sW8 b2]
  SCPSeg05Textbox bunk sid text voice counter ->
    mmconcat [sW8 0x05, sW8 bunk, sW32 sid, sBS text, sBS voice, sW32 counter]
  _          -> sW8 0x00

mmappend :: (Monad m, Monoid a) => m a -> m a -> m a
mmappend = liftA2 (<>)

mmconcat :: (Monad m, Monoid a) => [m a] -> m a
mmconcat = fmap mconcat . sequence

serializeBuilder :: Monad m => Builder -> m Bytes
serializeBuilder = return . BL.toStrict . BB.toLazyByteString

sW8 :: Monad m => Word8 -> m Builder
sW8 = return . BB.word8

sW32 :: MonadReader SCPOpts m => Word32 -> m Builder
sW32 w32 = reader scpOptEndianness >>= \case
  LittleEndian -> return $ BB.word32LE w32
  BigEndian    -> return $ BB.word32BE w32

sBS :: MonadReader SCPOpts m => Bytes -> m Builder
sBS bs = reader scpOptStringType >>= \case
  StrTyCString      -> return $ BB.byteString bs <> BB.word8 0x00
  StrTyLengthPrefix ->
    let lengthInt = BS.length bs
     in if   lengthInt > 255
        then error "can't serialize a textbox with text longer than 255 bytes in length prefix mode"
        else let lengthW8 = fromIntegral lengthInt :: Word8
              in return $ BB.word8 lengthW8 <> BB.byteString bs

{-
  SCPSeg01BG Bytes Word8 Word8
  SCPSeg02SFX Bytes Word8
  SCPSeg03 Word8 Bytes Word8
  SCPSeg04 Word8 Word8
  SCPSeg05Textbox Word8 Word32 Bytes Bytes Word32
  SCPSeg07SCP Bytes
  SCPSeg08
  SCPSeg09 Word8 [(Bytes, Word32)]
  SCPSeg0A Word8 Word8 Word32 Word32 Word32
  SCPSeg0B Word8 Word8
  SCPSeg0CFlag Word8 Word8
  SCPSeg0D Word8
  SCPSeg0E Word8
  SCPSeg0F
  SCPSeg10 Word8 Word8 Word8
  SCPSeg11EventCG Bytes
  SCPSeg12
  SCPSeg13 Word8 Bytes Word8 Word32
  SCPSeg14 Word8
  SCPSeg15
  SCPSeg16Wadai
  SCPSeg17 Word8 Word8
  SCPSeg18 Word8 Word8
  SCPSeg19 Word8 Word8
  SCPSeg1D
  SCPSeg1E Word8
  SCPSeg1FDelay
  SCPSeg20 Word8
  SCPSeg21
  SCPSeg30 Word8
  SCPSeg31
  SCPSeg32 Word32 Word8
  SCPSeg34 Word32
  SCPSeg3A Word8 Word8
  SCPSeg3C Word8 Word8
  SCPSeg41 Word8 Word32 Word32
  SCPSeg43SFX Bytes
  SCPSeg44SFX Bytes
  SCPSeg45SFX Bytes Word8
  SCPSeg46 Word8 Word8
  SCPSeg54 Word8 Word8
  SCPSeg55 Word8 Word8
  SCPSeg56 Word8 Word8
  SCPSeg5C Word8
  SCPSeg5D Word32
  SCPSeg5E Word32
  SCPSeg5F Word32
  SCPSeg60 Word32
  SCPSeg68
  SCPSeg6CWipe Word8 Word32 Word32 Word32
  SCPSeg6DWipe Word8 Word32 Word32 Word32
  SCPSeg6E
  SCPSeg6F
  SCPSeg70 Word8 Word8
  SCPSeg71 Word8 Word8
  SCPSeg72
  SCPSeg73Kyoro Word8 Word32
  SCPSeg74
  SCPSeg75 Word8
  SCPSeg77SCP Word8
-}
