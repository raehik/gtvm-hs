{-# LANGUAGE OverloadedStrings #-}

module GTVM.Assorted.SL01 where

import Codec.Compression.Lzo qualified as LZO
import Data.ByteString qualified as B

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.Magic
import Binrep.Type.ByteString

import Strongweak
import Strongweak.Generic

import GHC.Generics ( Generic )

data SL01 (s :: Strength) = SL01
  { sl01Magic            :: Magic "SL01"
  , sl01DecompressedSize :: SW s (I 'U 'I4 'LE)
  , sl01Data             :: SW s (AsByteString ('Pascal 'I4 'LE))
  } deriving stock (Generic)
deriving stock instance Show (SL01 'Strong)
deriving stock instance Eq   (SL01 'Strong)
deriving stock instance Show (SL01 'Weak)
deriving stock instance Eq   (SL01 'Weak)

instance BLen (SL01 'Strong) where blen = blenGeneric BR.cDef
instance Put  (SL01 'Strong) where put  = putGeneric  BR.cDef
instance Get  (SL01 'Strong) where get  = getGeneric  BR.cDef

instance Weaken (SL01 'Strong) where
    type Weak   (SL01 'Strong) = SL01 'Weak
    weaken = weakenGeneric
instance Strengthen (SL01 'Strong) where
    strengthen = strengthenGeneric

compress :: B.ByteString -> SL01 'Weak
compress bs = SL01 Magic (fromIntegral (B.length bs)) (LZO.compress bs)

decompress :: SL01 'Weak -> B.ByteString
decompress (SL01 _ s d) = LZO.decompress d (fromIntegral s)
