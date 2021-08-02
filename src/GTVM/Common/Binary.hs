{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTVM.Common.Binary where

import           Control.Monad.Reader

data BinaryCfg = BinaryCfg
  { binCfgEndianness :: Endianness
  , binCfgStringType :: StringType
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

-- | 'BinaryCfg' settings for original GTVM SCPs.
binCfgSCP :: BinaryCfg
binCfgSCP = BinaryCfg
  { binCfgEndianness = LittleEndian
  , binCfgStringType = StrTyCString
  }

-- | Helper for choosing actions based on configured endianness.
binCfgCaseEndianness :: MonadReader BinaryCfg m => m a -> m a -> m a
binCfgCaseEndianness fLE fBE = reader binCfgEndianness >>= \case
  LittleEndian -> fLE
  BigEndian    -> fBE

-- | Helper for choosing actions based on configured string type.
binCfgCaseStringType :: MonadReader BinaryCfg m => m a -> m a -> m a
binCfgCaseStringType fCStr fPascal = reader binCfgStringType >>= \case
  StrTyCString      -> fCStr
  StrTyLengthPrefix -> fPascal

-- | Attempt to convert one 'Integral' type into a 'Bounded' one.
--
-- Use with type applications e.g. @integralToBounded \@Word8 256@.
integralToBounded :: forall b a. (Integral a, Bounded b, Integral b) => a -> Maybe b
integralToBounded a =
    if   fromIntegral (minBound @b) <= a && a <= fromIntegral (maxBound @b)
    then Just (fromIntegral a)
    else Nothing
