{-| Various supporting code for parsing and serializing between binary formats.

Currently, I base my parsing & serializing on cereal, because it provides a
convenient set of combinators wrapped in a parse-serialize typeclass. However, I
want to shift away over time and write my own typeclass, because cereal's
'Serialize' is written to work with itself rather than foreign binary schemas,
so hides lots of implicit assumptions behind instances. What do I mean by this?

  * Ints and Words are serialized in big-endian. In the first place, they
    shouldn't have instances, because you can't serialize any multibyte type
    without knowing what byte order to use (apart from 'Word8' and 'Int8').
  * Lists are serialized length-prefixed using a big-endian 'Word64' (matching
    64-bit Haskell). My instance would instead serialize as-is, so parsing would
    read until EOF. You might think that's not very good for efficiency. Indeed,
    it isn't, and you don't want to design *your* binary schemas around that.
    But I want to write types that let me be explicit about how *others* are
    writing their schemas.

-}

{-# LANGUAGE OverloadedStrings, MagicHash #-}

module Raehik.Binary where

import GHC.Generics ( Generic )
import GHC.TypeNats
import GHC.Exts ( proxy#, Proxy# )
import Refined
import Refined.WithRefine
import Data.Typeable ( typeRep, Typeable )
import Util
import Data.Aeson
import Data.Serialize.Get
import Data.Serialize
import Data.Word
import Control.Monad ( replicateM )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as B
import Numeric.Natural
import GHC.Natural ( minusNaturalMaybe )

-- | The size in bytes of the type can be known, preferably on the cheap e.g.
--   reading a length field.
--
-- Aim to make this O(1). Except for where you can't like lists lol.
--
-- This is useful for padding types generically, without having to track the
-- read/write cursor. Yes, that feature *is* common and cheap for parsers (and
-- to lesser extent, serializers), but you should really only be padding types
-- that have a "static-ish" (= cheaply calculated) size. At least, I think so?
-- I'm not quite sure.
--
-- Some instances ignore the argument. It would be possible to pull those out
-- into a statically-known bytelength typeclass, but it wouldn't improve clarity
-- or performance, just get rid of a couple 'undefined's.
class ByteLength a where
    blen :: a -> Natural

instance ByteLength a => ByteLength [a] where blen = sum . map blen

-- | Byte order.
data Endianness
  = BE -- ^ big    endian, MSB first. e.g. most network protocols
  | LE -- ^ little endian, MSB last.  e.g. most processor architectures

-- | A type tagged with endianness. Intended to be used with the @Word@ family
--   of types.
newtype W (e :: Endianness) w = W { getW :: w }
    deriving stock   (Generic, Typeable, Show, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, Bounded, Num, Real, Enum, Integral, ToJSON, FromJSON)

instance ByteLength (W e Word16) where blen = const 2
instance ByteLength (W e Word32) where blen = const 4
instance ByteLength (W e Word64) where blen = const 8

-- | A C-style string: an arbitrary-length bytestring terminated by a null byte.
newtype CString = CString { getCString :: BS.ByteString }
    deriving stock   (Generic, Typeable, Show)
    deriving newtype (Eq)

instance ByteLength CString where
    blen cstr = fromIntegral $ BS.length (getCString cstr) + 1

-- | Total shite parsing efficiency. But, to be fair, that's why we don't
--   serialize arbitrary-length bytestrings!
instance Serialize CString where
    get = go mempty
      where
        go buf = do
            getWord8 >>= \case
              0x00    -> return $ CString $ BL.toStrict $ B.toLazyByteString buf
              nonNull -> go $ buf <> B.word8 nonNull
    put cstr = do
        putByteString $ getCString cstr
        putWord8 0x00

-- | Data is padded with null bytes to the given byte length.
--
-- Essentially a more specific 'Refined.SizeLessThan'.
data NullPadded (n :: Nat)

instance (KnownNat n, Foldable f) => Predicate (NullPadded n) (f a) where
    validate p a
      | length a > fromIntegral n
          = throwRefineOtherException (typeRep p) $
                   "Foldable is too long for given null-padded size: "
                <> tshow (length a) <> " > " <> tshow n
      | otherwise = success
      where n = natVal' (proxy# :: Proxy# n)

instance KnownNat n => Predicate (NullPadded n) CString where
    validate p cstr
      | len > n
          = throwRefineOtherException (typeRep p) $
                   "too long: "
                <> tshow len <> " > " <> tshow n <> " (includes null byte)"
      | otherwise = success
      where
        n = natVal' (proxy# :: Proxy# n)
        len = blen cstr

instance KnownNat n => ByteLength (WithRefine 'Enforced (NullPadded n) a) where
    blen = const n
      where n = natVal' (proxy# :: Proxy# n)

-- | predicate is inherently enforced due to checking length to calculate how
--   many succeeding nulls to parse
--
-- Note that the consumer probably doesn't care about the content of the
-- padding, just that the data is chunked correctly. I figure we care about
-- correctness here, so it'd be nice to know about the padding well-formedness
-- (i.e. that it's all nulls).
instance (Serialize a, ByteLength a, KnownNat n)
      => Serialize (WithRefine 'Enforced (NullPadded n) a) where
    get = do
        a <- get
        let len = blen a
        case minusNaturalMaybe n len of
          Nothing -> fail $ "too long: " <> show len <> " > " <> show n
          Just nullstrLen -> do
            getNNulls nullstrLen
            return $ reallyUnsafeEnforce a
      where
        n = natVal' (proxy# :: Proxy# n)
    put wrnpa = do
        let npa = unWithRefine wrnpa
        put npa
        let paddingLength = n - blen npa
        putByteString $ BS.replicate (fromIntegral paddingLength) 0x00
      where
        n = natVal' (proxy# :: Proxy# n)

getNNulls :: Natural -> Get ()
getNNulls = \case 0 -> return ()
                  n -> getWord8 >>= \case
                         0x00    -> getNNulls $ n-1
                         nonNull -> do
                           offset <- bytesRead
                           fail $  "expected null, found: "<> show nonNull
                                <> " at offset " <> show offset
                                <> ", " <> show n <> " more nulls to go"

{-
This is more efficient than doing regular NullPadded stuff to a CString, since
we can grab the whole chunk at the start then search for the first null, rather
than have to do explicit byte-per-byte parsing (and currently with messy
intermediates). But I'd have to do some overlapping stuff.

The solution is to write a special predicate @NullPaddedCString@ that doesn't
have the polymorphic instance, solely working on 'CString's. I'll do that
eventually, but for now, it kind of sucks to have to do so, so leaving it.

-- | predicate is inherently enforced due to getting the sized chunk
--
-- Note that we're extra careful when parsing and validate the null-padding too.
instance KnownNat n
      => Serialize (WithRefine 'Enforced (NullPadded n) CString) where
    get = do
        bs <- getByteString (fromIntegral n)
        let (truncated, remainder) = BS.span (/= 0x00) bs
        if   BS.all (== 0x00) remainder
        then return $ reallyUnsafeEnforce $ CString truncated
        else fail "null-padded CString had non-nulls in padding"
      where
        n = natVal' (proxy# :: Proxy# n)
    put wrcstr = do
        let bs = getCString $ unWithRefine wrcstr
        putByteString bs
        let paddingLength = fromIntegral n - BS.length bs
        putByteString $ BS.replicate paddingLength 0x00
      where
        n = natVal' (proxy# :: Proxy# n)
-}

-- | Data is prefixed with its length, given by a bounded type. Thus, it must be
--   shorter than the length prefix type maximum value.
--
-- Intended to be used with unsigned machine integers like 'Word32'. Or better,
-- ones explicitly tagged with their endianness like @'W' ''LE' 'Word32'@.
data LengthPrefixed w

instance (Bounded w, Integral w, Typeable w, Show w, Foldable f)
      => Predicate (LengthPrefixed w) (f a) where
    validate p a
      | length a > fromIntegral max'
          = throwRefineOtherException (typeRep p) $
                   "Foldable is too long for given length prefix type: "
                <> tshow (length a) <> " > " <> tshow max'
      | otherwise = success
      where max' = maxBound @w

-- | Safe because LengthPrefixed uses statically-sized length prefixes, so
--   'blen' doesn't evaluate its argument.
instance (ByteLength w, ByteLength a)
      => ByteLength (WithRefine 'Enforced (LengthPrefixed w) a) where
    blen wrlpa = blen (undefined :: w) + blen (unWithRefine wrlpa)

-- | predicate is inherently enforced due to using @w@ with 'replicateM'
instance (Serialize w, Integral w, Serialize a)
      => Serialize (WithRefine 'Enforced (LengthPrefixed w) [a]) where
    get = do
        len <- get @w
        as <- replicateM (fromIntegral len) (get @a)
        return $ reallyUnsafeEnforce as
    put wras = do
        put @w $ fromIntegral $ length as
        mapM_ (put @a) as
      where as = unWithRefine wras

instance Serialize (W 'BE Word16) where get = W <$> getWord16be
                                        put = putWord16be . getW
instance Serialize (W 'LE Word16) where get = W <$> getWord16le
                                        put = putWord16le . getW
instance Serialize (W 'BE Word32) where get = W <$> getWord32be
                                        put = putWord32be . getW
instance Serialize (W 'LE Word32) where get = W <$> getWord32le
                                        put = putWord32le . getW
instance Serialize (W 'BE Word64) where get = W <$> getWord64be
                                        put = putWord64be . getW
instance Serialize (W 'LE Word64) where get = W <$> getWord64le
                                        put = putWord64le . getW

-- | List wrapper to work around some bad 'Serialize' instances.
newtype List a = List { getList :: [a] }
    deriving stock   (Generic, Typeable, Show, Functor, Foldable, Traversable)
    deriving newtype (Eq, Ord, ToJSON, FromJSON)

-- | Serialize: plop every element down in order. Parse? The reverse, meaning
--   we're only done when we see EOF. Yes, this is a bad design pattern, and
--   hopefully your format doesn't use it.
--
-- We have to use this 'List' newtype to sidestep Serialize's '[]' instance,
-- which serializes with an implicit big-endian Word32 length prefix.
instance Serialize a => Serialize (List a) where
    put as = mapM_ put as
    get = go []
      where
        go as = do
            a <- get
            isEmpty >>= \case
              True -> return $ List $ reverse $ a : as
              False -> go $ a : as
