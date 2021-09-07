{-# LANGUAGE OverloadedStrings #-}

module GTVM.Assorted.SL01
  ( compress
  , decompress
  , pSL01
  , sSL01
  ) where

import qualified Codec.Compression.Lzo.Block as LZO
import qualified Data.ByteString as BS
import           Control.Monad.Reader
import           Data.Void
import           Text.Megaparsec
import           Data.Word
import           GTVM.Common.Binary
import           GTVM.Common.Binary.Parse
import           GTVM.Common.Binary.Serialize

type Bytes = BS.ByteString

data SL01 = SL01 Word32 Bytes deriving (Eq, Show)

compress :: Bytes -> SL01
compress bs =
    case integralToBounded (BS.length bs) of
      Nothing     -> error "that's a YUGE file friend, no can do"
      Just lenW32 -> SL01 lenW32 (LZO.compress9 bs)

decompress :: SL01 -> Bytes
decompress (SL01 decompressedSize compressedBytes) =
    LZO.decompress compressedBytes (fromIntegral decompressedSize)

pSL01
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m SL01
pSL01 = do
    _ <- chunk "SL01"
    decompressedSize <- pW32
    compressedSize <- pW32
    compressedBytes <- takeP (Just (show compressedSize <> "-byte bytestring")) (fromIntegral compressedSize)
    eof
    return $ SL01 decompressedSize compressedBytes

sSL01 :: (MonadReader BinaryCfg m) => SL01 -> m Bytes
sSL01 = serialize bSL01

bSL01 :: MonadReader BinaryCfg m => SL01 -> m Builder
bSL01 (SL01 decompressedSize compressedBytes) =
    case integralToBounded (BS.length compressedBytes) of
      Nothing -> error $
        "that's a YUGE file friend, no can do..."
        <> " but how did you manage to create this data in the first place?"
        <> " if the compressed bytes exceeds Word32,"
        <> " the decompressed certainly does. How are you here"
      Just compressedSize ->
        concatM
            [ bBS' "SL01"
            , bW32 decompressedSize
            , bW32 compressedSize
            , bBS' compressedBytes ]
