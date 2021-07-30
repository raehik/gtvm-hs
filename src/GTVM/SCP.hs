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
-- can't see an 0x06 case in code
  | SCPSeg07SCP Bytes
  | SCPSeg08
  | SCPSeg09 Word8 [(Bytes, Word32)]
  | SCPSeg0A Word8 Word8 Word32 Word32 Word32
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
  | SCPSeg1D
  | SCPSeg1E Word8
  | SCPSeg1FDelay
  | SCPSeg20 Word8
  | SCPSeg21
  | SCPSeg30 Word8
  | SCPSeg31
  | SCPSeg32 Word32 Word8
  | SCPSeg34 Word32
  | SCPSeg3A Word8 Word8
  | SCPSeg3C Word8 Word8
  | SCPSeg41 Word8 Word32 Word32
  | SCPSeg43SFX Bytes
  | SCPSeg44SFX Bytes
  | SCPSeg45SFX Bytes Word8
  | SCPSeg46 Word8 Word8
  | SCPSeg54 Word8 Word8
  | SCPSeg55 Word8 Word8
  | SCPSeg56 Word8 Word8
  | SCPSeg5C Word8
  | SCPSeg5D Word32
  | SCPSeg5E Word32
  | SCPSeg5F Word32
  | SCPSeg60 Word32
  | SCPSeg68
  | SCPSeg6CWipe Word8 Word32 Word32 Word32
  | SCPSeg6DWipe Word8 Word32 Word32 Word32
  | SCPSeg6E
  | SCPSeg6F
  | SCPSeg70 Word8 Word8
  | SCPSeg71 Word8 Word8
  | SCPSeg72
  | SCPSeg73Kyoro Word8 Word32
  | SCPSeg74
  | SCPSeg75 Word8
  | SCPSeg77SCP Word8
  -- ^ byte apparently should be between 1-6 inclusive
    deriving (Eq, Show)

data SCPOpts = SCPOpts
  { scpOptEndianness :: Endianness
  , scpOptStringType :: StringType
  } deriving (Eq, Show)

data Endianness
  = BigEndian
  | LittleEndian
    deriving (Eq, Show)

data StringType
  = StrTyCString
  -- ^ C strings: null-terminated.

  | StrTyLengthPrefix
  -- ^ Pascal strings: prefixed with length in bytes.
  --
  -- The game reads textboxes into a 256 byte buffer, so we follow suit and use
  -- a single byte to indicate length. TODO: make clearer in types, docs, code?
    deriving (Eq, Show)

scpOptsDef :: SCPOpts
scpOptsDef = SCPOpts
  { scpOptEndianness = LittleEndian
  , scpOptStringType = StrTyCString
  }
