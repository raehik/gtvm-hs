-- | Word of warning: we get pretty wacky over here.
--
-- TODO work on ordering, presentation of this module (perhaps split idk)

{-# LANGUAGE AllowAmbiguousTypes, OverloadedStrings #-}

module Raehik.Check where

import Data.Aeson
import GHC.Generics
import Data.Void
import Numeric.Natural
import Data.Text qualified as Text
import Data.Text ( Text )
import Data.ByteString qualified as BS
import Raehik.HexBytestring
import GHC.Exts ( Proxy#, proxy# )
import GHC.TypeLits ( KnownSymbol, symbolVal' )
import Text.Megaparsec
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

data Check
  = CheckEqual
  | CheckSize
  | CheckHash HashFunc
    deriving (Eq, Show, Generic)

type family CheckRep (c :: Check) a where
    CheckRep 'CheckEqual a    = a
    CheckRep 'CheckSize  _    = Natural
    CheckRep ('CheckHash h) _ = Hash h

data HashFunc
  = HashFuncB3
  | HashFuncSHA256
  | HashFuncMD5
    deriving (Eq, Show, Generic)

-- | A bytestring representing the output of hashing something using the given
--   hash function.
newtype Hash (h :: HashFunc) = Hash { hashBytes :: BS.ByteString }
    deriving (Eq, Show, Generic)

-- may as well do it at type level lol
type family HashFuncLabel (h :: HashFunc) where
    HashFuncLabel 'HashFuncB3     = "b3"
    HashFuncLabel 'HashFuncSHA256 = "sha256"
    HashFuncLabel 'HashFuncMD5    = "md5"

-- ...but we will need to reflect the 'Symbol' to value level
hashFuncLabel :: forall h l. (l ~ HashFuncLabel h, KnownSymbol l) => Text
hashFuncLabel = Text.pack (symbolVal' (proxy# :: Proxy# l))

-- | Pretty print a hash like @hashfunc:123abc@.
prettyHash :: forall h l. (l ~ HashFuncLabel h, KnownSymbol l) => Hash h -> Text
prettyHash h =
       hashFuncLabel @h
    <> Text.singleton ':'
    <> prettyHexBytestringCompact (hashBytes h)

-- | Add a @hash:@ prefix to better separate from regular text.
instance (l ~ HashFuncLabel h, KnownSymbol l) => ToJSON   (Hash h) where
    toJSON = String . Text.append "hash:" . prettyHash

instance (l ~ HashFuncLabel h, KnownSymbol l) => FromJSON (Hash h) where
    parseJSON = withText "hash string" $ \t ->
        case parseMaybe @Void parseHash t of
          Nothing   -> fail "failed to parse hash"
          Just hash -> pure hash

-- Bad naming, these maybe aren't symbols/lexemes in the expected sense
-- (horizontal spacing is optional).
parseHash
    :: forall h l e s m
    .  (l ~ HashFuncLabel h, KnownSymbol l, MonadParsec e s m, Token s ~ Char, Tokens s ~ Text)
    => m (Hash h)
parseHash = do
    symbol "hash"
    symbol ":"
    symbol $ hashFuncLabel @h
    symbol ":"
    bs <- parseHexBytestring
    return $ Hash bs

symbol :: (MonadParsec e s m, Token s ~ Char) => Tokens s -> m ()
symbol s = MCL.lexeme MC.hspace (chunk s) >> return ()
