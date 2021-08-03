{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTVM.Common.Binary.Serialize
  ( bW8
  , bW32
  , bW64
  , bBS
  , bBSNullPadTo
  , bBS'
  , bCString
  , bPascal
  , bCount
  , bNullPadTo
  , Builder
  , serialize
  , execBuilder
  , concatM
  ) where

import           GTVM.Common.Binary
import qualified Data.ByteString            as BS
import           Data.Word
import           Control.Monad.Reader
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL

type Builder = BB.Builder
type Bytes = BS.ByteString

bW8 :: Monad m => Word8 -> m Builder
bW8 = return . BB.word8

bW32 :: MonadReader BinaryCfg m => Word32 -> m Builder
bW32 w32 = binCfgCaseEndianness (return $ BB.word32LE w32) (return $ BB.word32BE w32)

bW64 :: MonadReader BinaryCfg m => Word64 -> m Builder
bW64 w64 = binCfgCaseEndianness (return $ BB.word64LE w64) (return $ BB.word64BE w64)

bBS :: MonadReader BinaryCfg m => Bytes -> m Builder
bBS bs = binCfgCaseStringType (bCString bs) (bPascal bs)

bCString :: Monad m => Bytes -> m Builder
bCString bs = concatM [bBS' bs, bW8 0x00]

bPascal :: Monad m => Bytes -> m Builder
bPascal bs =
    case integralToBounded (BS.length bs) of
      Nothing  -> error "can't serialize bytestring longer than 255 bytes in length prefix mode"
      Just len -> concatM [bW8 len, bBS' bs]

bBS' :: Monad m => Bytes -> m Builder
bBS' = return . BB.byteString

bBSNullPadTo :: Monad m => Bytes -> Int -> m Builder
bBSNullPadTo bs = bNullPadTo (bBS' bs)

-- TODO: This sucks. We're forced to do this because 'Data.ByteString.Builder'
-- doesn't let you view the current buffer size (it should be trivial!). I could
-- write my own builder monad that tracks it, but cba.
bNullPadTo :: Monad m => m Builder -> Int -> m Builder
bNullPadTo b padTo = do
    b' <- b
    let bs = execBuilder b'
        len = BS.length bs
        reqNulls = padTo - len
    if   reqNulls < 0
    then error $ "oversized field: " <> show len <> " > " <> show padTo
    else concatM [bBS' bs, bBS' (BS.replicate reqNulls 0x00)]

bCount
    :: forall m a b. (Monad m, Bounded a, Integral a, Show a)
    => (a -> m Builder) -> (b -> m Builder) -> [b] -> m Builder
bCount bl bb parts =
    let len = length parts
     in case integralToBounded len of
          Nothing   -> error $ "length not bounded: " <> show (minBound @a) <> "<=" <> show len <> "<=" <> show (maxBound @a) <> " does not hold"
          Just len' -> concatM $ bl len' : (bb <$> parts)

execBuilder :: Builder -> Bytes
execBuilder = BL.toStrict . BB.toLazyByteString

serialize :: Functor f => (a -> f Builder) -> a -> f Bytes
serialize f a = execBuilder <$> f a

concatM :: (Monad m, Monoid a) => [m a] -> m a
concatM as = mconcat <$> sequence as
