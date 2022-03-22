module GTVM.SCP.Serialize
  ( sSCP
  ) where

import GTVM.SCP
import GTVM.Common.Binary
import GTVM.Common.Binary.Serialize
import Data.Word
import Control.Monad.Reader
import Control.Applicative
import Data.ByteString.Builder qualified as BB
import Data.ByteString qualified as BS

type Bytes = BS.ByteString

sSCP :: MonadReader BinaryCfg m => [SCPSeg Bytes] -> m Bytes
sSCP = serialize bSCP

bBSW32 :: MonadReader BinaryCfg m => (Bytes, Word32) -> m Builder
bBSW32 (bs, u) = liftA2 (<>) (bBS bs) (bW32 u)

bSCP :: MonadReader BinaryCfg m => [SCPSeg Bytes] -> m Builder
bSCP = concatM . fmap bSCPSeg

bSCPSeg :: MonadReader BinaryCfg m => SCPSeg Bytes -> m Builder
bSCPSeg = \case
  SCPSeg00 -> r1 0x00
  SCPSeg01BG bs1 b1 b2 -> rB [bW8 0x01, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg02SFX bs1 b1 -> rB [bW8 0x02, bBS bs1, bW8 b1]
  SCPSeg03 b1 bs1 b2 -> rB [bW8 0x03, bW8 b1, bBS bs1, bW8 b2]
  SCPSeg04 b1 b2 -> rB [bW8 0x04, bW8 b1, bW8 b2]
  SCPSeg05Textbox (SCPSeg05Textbox' b1 u1 bs1 bs2 u2) -> rB [bW8 0x05, bW8 b1, bW32 u1, bBS bs1, bBS bs2, bW32 u2]
  -- no 0x06
  SCPSeg07SCP bs1 -> rB [bW8 0x07, bBS bs1]
  SCPSeg08 -> r1 0x08
  SCPSeg09Choice b1 bsW32Pairs -> rB [bW8 0x09, bW8 b1, bCount bW8 bBSW32 bsW32Pairs]
  SCPSeg0A b1 b2 u1 u2 u3 -> rB [bW8 0x0A, bW8 b1, bW8 b2, bW32 u1, bW32 u2, bW32 u3]
  SCPSeg0B b1 b2 -> rB [bW8 0x0B, bW8 b1, bW8 b2]
  SCPSeg0CFlag b1 b2 -> rB [bW8 0x0C, bW8 b1, bW8 b2]
  SCPSeg0D b1 -> rB [bW8 0x0D, bW8 b1]
  SCPSeg0E b1 -> rB [bW8 0x0E, bW8 b1]
  SCPSeg0F -> r1 0x0F
  SCPSeg10 b1 b2 b3 -> rB [bW8 0x10, bW8 b1, bW8 b2, bW8 b3]
  SCPSeg11EventCG bs1 -> rB [bW8 0x11, bBS bs1]
  SCPSeg12 -> r1 0x12
  SCPSeg13 b1 bs1 b2 u1 -> rB [bW8 0x13, bW8 b1, bBS bs1, bW8 b2, bW32 u1]
  SCPSeg14 b1 -> rB [bW8 0x14, bW8 b1]
  SCPSeg15 -> r1 0x15
  SCPSeg16Wadai -> r1 0x16
  SCPSeg17 b1 b2 -> rB [bW8 0x17, bW8 b1, bW8 b2]
  SCPSeg18 b1 b2 -> rB [bW8 0x18, bW8 b1, bW8 b2]
  SCPSeg19 b1 b2 -> rB [bW8 0x19, bW8 b1, bW8 b2]
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  SCPSeg1D -> r1 0x1D
  SCPSeg1E b1 -> rB [bW8 0x1E, bW8 b1]
  SCPSeg1FDelay -> r1 0x1F
  SCPSeg20 b1 -> rB [bW8 0x20, bW8 b1]
  SCPSeg21 -> r1 0x21
  SCPSeg22 bs1 bsW32Pairs -> rB [bW8 0x22, bBS bs1, bCount bW8 bBSW32 bsW32Pairs]
  SCPSeg23SFX -> r1 0x23
  SCPSeg24 bs1 -> rB [bW8 0x24, bBS bs1]
  SCPSeg25 -> r1 0x25
  SCPSeg26 -> r1 0x26
  SCPSeg27 bs1 b1 b2 -> rB [bW8 0x27, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg28 bs1 b1 b2 -> rB [bW8 0x28, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg29 bs1 b1 b2 -> rB [bW8 0x29, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg2A b1 b2 -> rB [bW8 0x2A, bW8 b1, bW8 b2]
  SCPSeg2B b1 b2 b3 bs1 b4 -> rB [bW8 0x2B, bW8 b1, bW8 b2, bW8 b3, bBS bs1, bW8 b4]
  SCPSeg2CMap -> r1 0x2C
  SCPSeg2D bs1 b1 b2 -> rB [bW8 0x2D, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg2E b1 b2 u1 u2 -> rB [bW8 0x2E, bW8 b1, bW8 b2, bW32 u1, bW32 u2]
  SCPSeg2F b1 -> rB [bW8 0x2F, bW8 b1]
  SCPSeg30 b1 -> rB [bW8 0x30, bW8 b1]
  SCPSeg31 -> r1 0x31
  SCPSeg32 u1 b1 -> rB [bW8 0x32, bW32 u1, bW8 b1]
  SCPSeg33 b1 b2 -> rB [bW8 0x33, bW8 b1, bW8 b2]
  SCPSeg34 u1 -> rB [bW8 0x34, bW32 u1]
  SCPSeg35 bs1 -> rB [bW8 0x35, bBS bs1]
  SCPSeg36 b1 u1 -> rB [bW8 0x36, bW8 b1, bW32 u1]
  SCPSeg37 b1 u1 -> rB [bW8 0x37, bW8 b1, bW32 u1]
  SCPSeg38 b1 u1 -> rB [bW8 0x38, bW8 b1, bW32 u1]
  SCPSeg39 -> r1 0x39
  SCPSeg3A b1 b2 -> rB [bW8 0x3A, bW8 b1, bW8 b2]
  SCPSeg3B b1 -> rB [bW8 0x3B, bW8 b1]
  SCPSeg3C b1 b2 -> rB [bW8 0x3C, bW8 b1, bW8 b2]
  SCPSeg3D -> r1 0x3D
  SCPSeg3E b1 b2 -> rB [bW8 0x3E, bW8 b1, bW8 b2]
  SCPSeg3F b1 u1 u2 -> rB [bW8 0x3F, bW8 b1, bW32 u1, bW32 u2]
  SCPSeg40 b1 -> rB [bW8 0x40, bW8 b1]
  SCPSeg41 b1 u1 u2 -> rB [bW8 0x41, bW8 b1, bW32 u1, bW32 u2]
  SCPSeg42 b1 -> rB [bW8 0x42, bW8 b1]
  SCPSeg43SFX bs1 -> rB [bW8 0x43, bBS bs1]
  SCPSeg44SFX bs1 -> rB [bW8 0x44, bBS bs1]
  SCPSeg45SFX bs1 b1 -> rB [bW8 0x45, bBS bs1, bW8 b1]
  SCPSeg46 b1 b2 -> rB [bW8 0x46, bW8 b1, bW8 b2]
  SCPSeg47 b1 b2 -> rB [bW8 0x47, bW8 b1, bW8 b2]
  SCPSeg48 -> r1 0x48
  SCPSeg49 -> r1 0x49
  SCPSeg4A -> r1 0x4A
  SCPSeg4B -> r1 0x4B
  SCPSeg4C bs1 -> rB [bW8 0x4C, bBS bs1]
  SCPSeg4D -> r1 0x4D
  SCPSeg4E -> r1 0x4E
  SCPSeg4F bs1 b1 b2 -> rB [bW8 0x4F, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg50 bs1 b1 b2 -> rB [bW8 0x50, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg51 bs1 b1 b2 -> rB [bW8 0x51, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg52 bs1 -> rB [bW8 0x52, bBS bs1]
  SCPSeg53 bs1 b1 b2 -> rB [bW8 0x53, bBS bs1, bW8 b1, bW8 b2]
  SCPSeg54 b1 b2 -> rB [bW8 0x54, bW8 b1, bW8 b2]
  SCPSeg55 b1 b2 -> rB [bW8 0x55, bW8 b1, bW8 b2]
  SCPSeg56 b1 b2 -> rB [bW8 0x56, bW8 b1, bW8 b2]
  SCPSeg57 b1 b2 -> rB [bW8 0x57, bW8 b1, bW8 b2]
  SCPSeg58 b1 b2 -> rB [bW8 0x58, bW8 b1, bW8 b2]
  SCPSeg59 b1 -> rB [bW8 0x59, bW8 b1]
  SCPSeg5A u1 b1 -> rB [bW8 0x5A, bW32 u1, bW8 b1]
  SCPSeg5B u1 -> rB [bW8 0x5B, bW32 u1]
  SCPSeg5C b1 -> rB [bW8 0x5C, bW8 b1]
  SCPSeg5D u1 -> rB [bW8 0x5D, bW32 u1]
  SCPSeg5E u1 -> rB [bW8 0x5E, bW32 u1]
  SCPSeg5F u1 -> rB [bW8 0x5F, bW32 u1]
  SCPSeg60 u1 -> rB [bW8 0x60, bW32 u1]
  SCPSeg61 b1 b2 -> rB [bW8 0x61, bW8 b1, bW8 b2]
  SCPSeg62 b1 -> rB [bW8 0x62, bW8 b1]
  SCPSeg63 -> r1 0x63
  SCPSeg64 -> r1 0x64
  SCPSeg65Trophy -> r1 0x65
  SCPSeg66 -> r1 0x66
  SCPSeg67 -> r1 0x67
  SCPSeg68 -> r1 0x68
  SCPSeg69 -> r1 0x69
  SCPSeg6A b1 -> rB [bW8 0x6A, bW8 b1]
  SCPSeg6B b1 -> rB [bW8 0x6B, bW8 b1]
  SCPSeg6CWipe b1 u1 u2 u3 -> rB [bW8 0x6C, bW8 b1, bW32 u1, bW32 u2, bW32 u3]
  SCPSeg6DWipe b1 u1 u2 u3 -> rB [bW8 0x6D, bW8 b1, bW32 u1, bW32 u2, bW32 u3]
  SCPSeg6E -> r1 0x6E
  SCPSeg6F -> r1 0x6F
  SCPSeg70 b1 b2 -> rB [bW8 0x70, bW8 b1, bW8 b2]
  SCPSeg71 b1 b2 -> rB [bW8 0x71, bW8 b1, bW8 b2]
  SCPSeg72 -> r1 0x72
  SCPSeg73Kyoro b1 u1 -> rB [bW8 0x73, bW8 b1, bW32 u1]
  SCPSeg74 -> r1 0x74
  SCPSeg75 b1 -> rB [bW8 0x75, bW8 b1]
  SCPSeg76 -> r1 0x76
  SCPSeg77SCP b1 -> rB [bW8 0x77, bW8 b1]

r1 :: Monad m => Word8 -> m Builder
r1 = return . BB.word8
rB :: Monad m => [m Builder] -> m Builder
rB = concatM
