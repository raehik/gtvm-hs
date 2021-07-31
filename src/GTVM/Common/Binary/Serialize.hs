{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTVM.Common.Binary.Serialize
  ( Builder
  , serialize
  , execBuilder
  , concatM
  , bW8
  , bW32
  , bW64
  , bBS
  , bBSFixedNullPadded
  , bCount
  ) where

import           GTVM.Common.Binary
import qualified Data.ByteString            as BS
import           Data.Word
import           Control.Monad.Reader
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
--import           ByteString.StrictBuilder

type Builder = BB.Builder
type Bytes = BS.ByteString

execBuilder :: Builder -> Bytes
execBuilder = BL.toStrict . BB.toLazyByteString

serialize :: Functor f => (a -> f Builder) -> a -> f Bytes
serialize f a = execBuilder <$> f a

concatM :: (Monad m, Monoid a) => [m a] -> m a
concatM as = mconcat <$> sequence as

bW8 :: Monad m => Word8 -> m Builder
bW8 = return . BB.word8

bW32 :: MonadReader BinaryCfg m => Word32 -> m Builder
bW32 w32 = binCfgCaseEndianness (return $ BB.word32LE w32) (return $ BB.word32BE w32)

bW64 :: MonadReader BinaryCfg m => Word64 -> m Builder
bW64 w64 = binCfgCaseEndianness (return $ BB.word64LE w64) (return $ BB.word64BE w64)

bBS :: MonadReader BinaryCfg m => Bytes -> m Builder
bBS bs = reader binCfgStringType >>= \case
  StrTyCString      -> return $ BB.byteString bs <> BB.word8 0x00
  StrTyLengthPrefix ->
    let len = BS.length bs
     in if   len > 255
        then error "can't serialize a textbox with text longer than 255 bytes in length prefix mode"
        else let lenW8 = fromIntegral len :: Word8
              in return $ BB.word8 lenW8 <> BB.byteString bs

bBSFixedNullPadded :: Monad m => Int -> Bytes -> m Builder
bBSFixedNullPadded i bs =
    let reqNulls = i - BS.length bs
     in if   reqNulls < 0
        then error $ "bytestring too large for fixed-size field (" <> show (BS.length bs) <> " > " <> show i <> ")"
        else return $ BB.byteString bs <> BB.byteString (BS.replicate reqNulls 0x00)

bCount
    :: forall m a b. (Monad m, Bounded a, Integral a, Show a)
    => (a -> m Builder) -> (b -> m Builder) -> [b] -> m Builder
bCount bl bb parts =
    let len = length parts
     in if   wellBounded len
        then error $ "length not bounded: " <> show (minBound @a) <> "<=" <> show len <> "<=" <> show (maxBound @a) <> " does not hold"
        else concatM $ bl (fromIntegral len) : (bb <$> parts)
  where
    -- This is interesting: I *require* scoped type vars and having 'a' in scope
    -- in order to write this simply. I need 'a' in scope to use in a type
    -- application, but this function doesn't actually have 'a' in the type
    -- signature (only the constraints), so apparently I can't write it as a
    -- top-level definition.
    -- TODO: perhaps ask on #haskell or somewhere, to gain better understanding
    wellBounded :: forall b'. (Integral b') => b' -> Bool
    wellBounded x = fromIntegral (minBound @a) <= x && x <= fromIntegral (maxBound @a)
