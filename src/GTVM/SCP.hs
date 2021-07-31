module GTVM.SCP where

import qualified Data.ByteString            as BS
import           Data.Word

{-
class HasSCPRep a where
    scpBytes :: a -> BS.ByteString
-}

type Bytes = BS.ByteString

-- TODO: Annotate data with meaning. We shouldn't hardcode this into the AST
-- during parsing like `SCPStr Bytes UserFacing`, but how to do it post-parse?

-- | A standalone segment of an SCP file. Essentially tokens from a byte lexer.
data SCPSegment
  = SCPSeg00
  | SCPSeg01BG Bytes Word8 Word8
  | SCPSeg02SFX Bytes Word8
  | SCPSeg03 Word8 Bytes Word8
  | SCPSeg04 Word8 Word8
  | SCPSeg05Textbox Word8 Word32 Bytes Bytes Word32
  -- no 0x06
  | SCPSeg07SCP Bytes
  | SCPSeg08
  | SCPSeg09 Word8 [(Bytes, Word32)]
  | SCPSeg0A Word8 Word8 Word32 Word32 Word32
  -- ^ TODO: decomp code a bit confusing for this command, be aware
  | SCPSeg0B Word8 Word8
  | SCPSeg0CFlag Word8 Word8
  | SCPSeg0D Word8
  | SCPSeg0E Word8
  | SCPSeg0F
  | SCPSeg10 Word8 Word8 Word8
  | SCPSeg11EventCG Bytes
  | SCPSeg12
  | SCPSeg13 Word8 Bytes Word8 Word32
  | SCPSeg14 Word8
  | SCPSeg15
  | SCPSeg16Wadai
  | SCPSeg17 Word8 Word8
  | SCPSeg18 Word8 Word8
  | SCPSeg19 Word8 Word8
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  | SCPSeg1D
  | SCPSeg1E Word8
  | SCPSeg1FDelay
  | SCPSeg20 Word8
  | SCPSeg21
  | SCPSeg22 Bytes [(Bytes, Word32)]
  | SCPSeg23SFX
  | SCPSeg24 Bytes
  | SCPSeg25
  | SCPSeg26
  | SCPSeg27 Bytes Word8 Word8
  | SCPSeg28 Bytes Word8 Word8
  | SCPSeg29 Bytes Word8 Word8
  | SCPSeg2A Word8 Word8
  | SCPSeg2B Word8 Word8 Word8 Bytes Word8
  | SCPSeg2CMap
  | SCPSeg2D Bytes Word8 Word8
  | SCPSeg2E Word8 Word8 Word32 Word32
  | SCPSeg2F Word8
  | SCPSeg30 Word8
  | SCPSeg31
  | SCPSeg32 Word32 Word8
  | SCPSeg33 Word8 Word8
  | SCPSeg34 Word32
  | SCPSeg35 Bytes
  | SCPSeg36 Word8 Word32
  | SCPSeg37 Word8 Word32
  | SCPSeg38 Word8 Word32
  | SCPSeg39
  | SCPSeg3A Word8 Word8
  | SCPSeg3B Word8
  | SCPSeg3C Word8 Word8
  | SCPSeg3D
  | SCPSeg3E Word8 Word8
  | SCPSeg3F Word8 Word32 Word32
  | SCPSeg40 Word8
  | SCPSeg41 Word8 Word32 Word32
  | SCPSeg42 Word8
  -- these don't appear very SFXy in code, but did in data
  | SCPSeg43SFX Bytes
  | SCPSeg44SFX Bytes
  | SCPSeg45SFX Bytes Word8
  | SCPSeg46 Word8 Word8
  | SCPSeg47 Word8 Word8
  | SCPSeg48
  | SCPSeg49
  | SCPSeg4A
  | SCPSeg4B
  | SCPSeg4C Bytes
  | SCPSeg4D
  | SCPSeg4E
  | SCPSeg4F Bytes Word8 Word8
  | SCPSeg50 Bytes Word8 Word8
  | SCPSeg51 Bytes Word8 Word8
  | SCPSeg52 Bytes
  | SCPSeg53 Bytes Word8 Word8
  | SCPSeg54 Word8 Word8
  | SCPSeg55 Word8 Word8
  | SCPSeg56 Word8 Word8
  | SCPSeg57 Word8 Word8
  | SCPSeg58 Word8 Word8
  | SCPSeg59 Word8
  | SCPSeg5A Word32 Word8
  | SCPSeg5B Word32
  | SCPSeg5C Word8
  | SCPSeg5D Word32
  | SCPSeg5E Word32
  | SCPSeg5F Word32
  | SCPSeg60 Word32
  | SCPSeg61 Word8 Word8
  | SCPSeg62 Word8 -- 0x01 <= w8 <= 0x11, no default
  | SCPSeg63
  | SCPSeg64
  | SCPSeg65Trophy
  | SCPSeg66
  | SCPSeg67
  | SCPSeg68
  | SCPSeg69
  | SCPSeg6A Word8
  | SCPSeg6B Word8
  | SCPSeg6CWipe Word8 Word32 Word32 Word32
  | SCPSeg6DWipe Word8 Word32 Word32 Word32
  | SCPSeg6E
  | SCPSeg6F
  | SCPSeg70 Word8 Word8
  | SCPSeg71 Word8 Word8
  | SCPSeg72
  | SCPSeg73Kyoro Word8 Word32 -- 0x01 <= w8 <= 0x06, with default
  | SCPSeg74
  | SCPSeg75 Word8
  | SCPSeg76
  | SCPSeg77SCP Word8
    deriving (Eq, Show)
