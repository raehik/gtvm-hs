module Config where

import           Data.Word
import           LinearPatch ( CPatch )

data ToolGroup
  = TGFlowchart CJSON (CStream, CStream) CParseType
  | TGSCP CJSON (CStream, CStream)
  | TGSL01 CBin (CStream, CStream)
  | TGPak CPak Bool
  | TGPatch CPatch FilePath (CStream, CStream) Bool CPatchType
  | TGCSVPatch (CStream, CStream)
    deriving (Eq, Show)

data CPatchType = CPatchTypeBin | CPatchTypeString
    deriving (Eq, Show)

-- | orig one bin stream <-> one text stream
--
-- Means printStdout only applies to encoder.
data CJSON
  = CJSONDe Bool
  -- ^ bool = prettify
  | CJSONEn Bool
  -- ^ bool = print binary to stdout
    deriving (Eq, Show)

-- | orig one bin stream <-> one bin stream
data CBin = CBin
  { _cBinCDirection     :: CDirection
  , _cBinAllowBinStdout :: Bool
  } deriving (Eq, Show)

-- | archive tool (one <-> many)
--
-- It's difficult to put the streams into a parent data type, because they
-- actually change meaning between directions (and in a way that, unlike dealing
-- with 1<->1, I want to change the help text for the CLI). So I stuff them
-- here. Interesting!
data CPak
  = CPakUnpack (CStream, CStreams)
  | CPakPack (CStream, CStreams) Word32
  -- ^ W32 = unknown value
    deriving (Eq, Show)

-- | "Single file" stream.
data CStream
  = CStreamFile FilePath
  | CStreamStd
    deriving (Eq, Show)

-- | "Multi-file" stream.
--
-- For things that don't make sense to reserialize as JSON or similar (i.e.
-- archives). Essentially anything that accepts '[(FilePath, ByteString)]'.
data CStreams
  = CStreamsFolder FilePath
  | CStreamsArchive FilePath
    deriving (Eq, Show)

-- | Direction from/to original.
--
-- Potentially useful for writing a less strict CLI, e.g. lots of switches
-- instead of separated commands. (I started with that, then moved to
-- command-based. Both are good.)
data CDirection
  = CDirectionFromOrig
  | CDirectionToOrig
    deriving (Eq, Show)

-- | Are we dealing with fully-parsed files, or only "partially" parsed?
--
-- "Partially parsed" files are kind of like Haskell data-tagged streams, a la
-- lexing. They should be relatively trivial to parse and serialize (and look
-- similar both ways). It seems a handy way to model binary formats.
data CParseType
  = CParseTypeFull
  | CParseTypePartial
    deriving (Eq, Show)
