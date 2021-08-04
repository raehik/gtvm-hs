module Config where

import           Data.Word

data ToolGroup
  = TGFlowchart CJSON CParseType
  | TGSCP CJSON
  | TGSL01 CBin
  | TGPak CPak
    deriving (Eq, Show)

data CStream
  = CStreamFile FilePath
  | CStreamStd
    deriving (Eq, Show)

data CStreams
  = CStreamsFolder FilePath
  | CStreamsArchive FilePath
    deriving (Eq, Show)

data CDirection
  = CDirectionFromOrig
  | CDirectionToOrig
    deriving (Eq, Show)

-- | one stream -> one stream
data CBin = CBin
  { _cBinCDirection     :: CDirection
  , _cBinCStream2       :: (CStream, CStream)
  , _cBinAllowBinStdout :: Bool
  } deriving (Eq, Show)

data CParseType
  = CParseTypeFull
  | CParseTypePartial
    deriving (Eq, Show)

data CPak
  = CPakUnpack CS1N Bool
  -- ^ bool = print binary to stdout
  | CPakPack CS1N Bool Word32
  -- ^ bool = print binary to stdout
    deriving (Eq, Show)

-- This was fun to recognise. With previous commands being 1<->1, I could keep
-- both on hand and use the same help text, swapping "how they're used" on the
-- code side. But 1<->n means that the 1 is always the same type, just changes
-- "position". Anyway, it's kind of opposite to what I was doing before, and
-- this is the solution, plus gives me clean, combinatory optparse code.
--
-- Wait, oops, I didn't need to make this its own data type, optparse is clever
-- enough. But eh, minor detail, and the type makes sense enough.
data CS1N = CS1N
  { _cS1N1 :: CStream
  , _cS1NN :: CStreams
  } deriving (Eq, Show)

data CJSON
  = CJSONDe (CStream, CStream) Bool
  -- ^ bool = prettify
  | CJSONEn (CStream, CStream) Bool
  -- ^ bool = print binary to stdout
    deriving (Eq, Show)
