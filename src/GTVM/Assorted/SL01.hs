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
import Binrep.Type.LenPfx

import Refined

import GHC.Generics ( Generic )

import GTVM.Common.Binary ( integralToBounded ) -- TODO move

-- | Don't construct values of this type manually, use the helper!
data SL01 = SL01
  { sl01Magic            :: Magic "SL01"
  , sl01DecompressedSize :: I 'U 'I4 'LE
  , sl01Data             :: AsByteString ('Pascal 'I4 'LE)
  } deriving (Generic, Eq, Show)

brCfgNoSum :: BR.Cfg (I 'U 'I1 'LE)
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

instance BLen SL01 where blen = blenGeneric brCfgNoSum
instance Put  SL01 where put  = putGeneric  brCfgNoSum
instance Get  SL01 where get  = getGeneric  brCfgNoSum

compress :: B.ByteString -> Either String SL01
compress bs = do
    case integralToBounded (B.length bs) of
      Nothing -> Left $ "TODO too long"
      Just lenW32 ->
        case refine (LZO.compress bs) of
          Left err -> Left $ show err
          Right bs' -> Right $ SL01 Magic lenW32 bs'

decompress :: SL01 -> B.ByteString
decompress sl01 =
    LZO.decompress compressedBs (fromIntegral (sl01DecompressedSize sl01))
  where compressedBs = unrefine $ sl01Data sl01
