{- TODO
  * support xxHash (XXH3_64bits)
    * as of 2022-08-30, there are 2 old unsupported libs... but I want XXH3, and
      I want as speedy an interface as possible. Not really up to it myself
      right now
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Raehik.Hash.Internal where

import Binrep.Extra.HexByteString

import GHC.Generics ( Generic )

import GHC.Exts ( Proxy#, proxy# )
import GHC.TypeLits ( Symbol, KnownSymbol, symbolVal' )

import Data.ByteString qualified as B
import Data.Word ( Word8 )
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Void ( Void )

-- Digest
import Data.Aeson qualified as Aeson
import Data.Aeson ( ToJSON(..), FromJSON(..) )

-- Digest parsing
import Text.Megaparsec
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL
import Control.Monad ( void )

class Hash h where
    -- | A unique string to identify the hash function by.
    --
    -- Used by the Aeson instances to serialize prettier JSON. You should
    -- decapitalize, e.g. SHA256 -> @sha256@.
    type HashFuncLabel h :: Symbol

    -- | Internal function, do not use.
    hash' :: B.ByteString -> B.ByteString

hashFuncLabel :: forall h l. (l ~ HashFuncLabel h, KnownSymbol l) => Text
hashFuncLabel = Text.pack (symbolVal' (proxy# :: Proxy# l))

--------------------------------------------------------------------------------

-- | The resulting digest from hashing some data using the given hash function.
--
-- TODO
-- As of 2022, most good cryptographic hash functions produce digest sizes
-- between 256-512 bits. That's 32-64 bytes. So I want to use a ShortByteString,
-- but the BLAKE3 library uses the memory library, which I can't figure out. I
-- bet it'd be more efficient. So, I'm polymorphising in preparation.
newtype Digest h a = Digest { getDigest :: a }
    deriving stock (Eq, Show, Generic)

-- | Add a @digest:@ prefix to better separate from regular text.
instance (l ~ HashFuncLabel h, KnownSymbol l) => ToJSON   (Digest h B.ByteString) where
    toJSON = Aeson.String . Text.append "digest:" . prettyDigest B.unpack

instance (l ~ HashFuncLabel h, KnownSymbol l) => FromJSON (Digest h B.ByteString) where
    parseJSON = Aeson.withText "hex hash digest" $ \t ->
        case parseMaybe @Void parseDigest t of
          Nothing -> fail "failed to parse hex hash digest"
          Just h  -> pure h

-- | Pretty print a hash like @hashfunc:123abc@.
prettyDigest
    :: forall h a l. (l ~ HashFuncLabel h, KnownSymbol l)
    => (a -> [Word8]) -> Digest h a -> Text
prettyDigest unpack (Digest d) =
       hashFuncLabel @h
    <> Text.singleton ':'
    <> prettyHexByteStringCompact unpack d

-- Bad naming, these maybe aren't symbols/lexemes in the expected sense
-- (horizontal spacing is optional).
parseDigest
    :: forall h l e s m
    .  (l ~ HashFuncLabel h, KnownSymbol l, MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)
    => m (Digest h B.ByteString)
parseDigest = do
    symbol "digest"
    symbol ":"
    symbol $ hashFuncLabel @h
    symbol ":"
    Digest <$> parseHexByteString B.pack
  where symbol = void . MCL.lexeme MC.hspace . chunk
