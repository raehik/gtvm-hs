{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Binary-format number parsers.
module Text.Megaparsec.Byte.Binary
  ( -- * Generic parsers
    anyFBits,
    anyLE,
    anyBE,

    -- * Word-specific parsers
    anyW16LE,
    anyW32LE,
    anyW64LE,
    anyW16BE,
    anyW32BE,
    anyW64BE,
    anyW8,
  )
where

import Data.Bits
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Word
import Text.Megaparsec

-- | Parse any byte-representable value from any 'Word8' token stream using the
--   given function.
--
-- Intended for byte-aligned types. May not work as expected with byte-unaligned
-- types (bitsize not a multiple of 8).
anyFBits ::
  forall a e s m.
  (MonadParsec e s m, Token s ~ Word8, FiniteBits a, Num a) =>
  ([Word8] -> a) ->
  Maybe String ->
  m a
anyFBits convert mLbl = do
  let reqLen = finiteByteSize @a
      lbl = show reqLen <> "-byte " <> fromMaybe "value" mLbl
  bytes <- takeP (Just lbl) reqLen
  let bytesList = chunkToTokens (Proxy :: Proxy s) bytes
  return (convert bytesList)

--------------------------------------------------------------------------------

-- | Parse a little-endian number from any 'Word8' token stream.
--
-- You may wish to call this with a visible type application:
--
--   > number <- anyLE @Word32
anyLE ::
  forall a e s m.
  (MonadParsec e s m, Token s ~ Word8, FiniteBits a, Num a) =>
  m a
anyLE = anyFBits bytesLE (Just "little-endian integer")

-- | Parse a big-endian number from any 'Word8' token stream.
--
-- You may wish to call this with a visible type application:
--
--   > number <- anyBE @Word32
anyBE ::
  forall a e s m.
  (MonadParsec e s m, Token s ~ Word8, FiniteBits a, Num a) =>
  m a
anyBE = anyFBits bytesBE (Just "big-endian integer")

--------------------------------------------------------------------------------

-- | Parse a little-endian Word16 from any 'Word8' token stream.
anyW16LE ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Word8) =>
  m Word16
anyW16LE = anyLE

-- | Parse a little-endian Word32 from any 'Word8' token stream.
anyW32LE ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Word8) =>
  m Word32
anyW32LE = anyLE

-- | Parse a little-endian Word64 from any 'Word8' token stream.
anyW64LE ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Word8) =>
  m Word64
anyW64LE = anyLE

-- | Parse a big-endian Word16 from any 'Word8' token stream.
anyW16BE ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Word8) =>
  m Word16
anyW16BE = anyBE

-- | Parse a big-endian Word32 from any 'Word8' token stream.
anyW32BE ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Word8) =>
  m Word32
anyW32BE = anyBE

-- | Parse a big-endian Word64 from any 'Word8' token stream.
anyW64BE ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Word8) =>
  m Word64
anyW64BE = anyBE

-- | Parse a Word8 from any 'Word8' token stream.
anyW8 ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Word8) =>
  m Word8
anyW8 = anySingle <?> "byte"

--------------------------------------------------------------------------------
-- Helpers

-- | Convert a 'Word8' list to a little-endian numeric type.
--
-- Does not check that the target type will fit the resulting value.
bytesBE :: (Bits a, Num a) => [Word8] -> a
bytesBE = foldl' go 0
  where
    go acc byte = (acc `shiftL` 8) .|. fromIntegral byte

-- | Parse a 'Word8' list as a big-endian number.
--
-- Does not check that the target type will fit the resulting value.
bytesLE :: (Bits a, Num a) => [Word8] -> a
bytesLE = fst . (foldl' go (0, 0))
  where
    go (acc, index) byte = (acc + x, index + 1)
      where
        x = fromIntegral byte `shiftL` (8 * index)

-- | Return the number of bytes in the argument.
--
-- Performs ceiling divison, so byte-unaligned types (bitsize not a multiple of
-- 8) should work, but further usage is not tested.
finiteByteSize :: forall a. (FiniteBits a) => Int
finiteByteSize = finiteBitSize @a undefined `ceilDiv` 8

ceilDiv :: Integral a => a -> a -> a
ceilDiv x y = (x + y -1) `div` y
