{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}

module GTVM.Assorted.Pak
  ( Pak(..)
  , PakHeader(..)
  , PakHeaderFTE(..)
  , pPakHeader
  , sPak
  ) where

import           GTVM.Common.Orphans()
import           GTVM.Common.Binary
import           GTVM.Common.Binary.Parse
import           GTVM.Common.Binary.Serialize
import           Text.Megaparsec
import qualified Data.ByteString as BS
import           Data.Void
import           Data.Word
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import           Control.Monad.State

import           Data.Aeson
import           GHC.Generics

-- TODO: maybe parse trailing stuff (nulls?) between file table and files

type Bytes = BS.ByteString

data Pak = Pak Word32 [(Text, Bytes)] deriving (Eq, Show, Generic)
data PakHeader = PakHeader Word32 [PakHeaderFTE] deriving (Eq, Show, Generic)
data PakHeaderFTE = PakHeaderFTE Word32 Word32 Bytes deriving (Eq, Show, Generic)

pPakHeader
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m PakHeader
pPakHeader = do
    numEntries <- pW32
    unkW32 <- pW32
    filetable <- count (fromIntegral numEntries) pPakHeaderFTE
    return $ PakHeader unkW32 filetable

pPakHeaderFTE
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m PakHeaderFTE
pPakHeaderFTE = PakHeaderFTE <$> pW32 <*> pW32 <*> pBSTextFixed 0x18

sPak :: MonadReader BinaryCfg m => Pak -> m Bytes
sPak = serialize bPak

bPak :: MonadReader BinaryCfg m => Pak -> m Builder
bPak (Pak unk files) = concatM [bW32 (fromIntegral (length files)), bW32 unk, bPakMain files]

bPakMain :: MonadReader BinaryCfg m => [(Text, Bytes)] -> m Builder
bPakMain x = evalStateT (go x) (mempty, mempty, totalFTSize)
  where
    totalFTSize = 0x08 + (length x * 0x20) + 0x18 -- TODO yeah I dunno why
    go = \case
      [] -> do
        (bFT, bFs, _) <- get
        bSomeSpacingDunno <- bBSNullPadTo "" 0x18
        return $ bFT <> bSomeSpacingDunno <> bFs
      ((f,bs):fs) -> do
        (bFT, bFs, offset) <- get
        bOffset <- bW32 (fromIntegral offset)
        bLen    <- bW32 (fromIntegral (BS.length bs))
        bF      <- bBSNullPadTo (Text.encodeUtf8 f) 0x18
        bFSpacingDunno <- bBSNullPadTo "" 0x1C
        builtBS <- bBS' bs
        let bFT' = bFT <> bOffset <> bLen <> bF
            bFs' = bFs <> builtBS <> bFSpacingDunno
            offset' = offset + (BS.length bs) + 0x1C
        put (bFT', bFs', offset')
        go fs

-- TODO: How do we serialize something like this nicely? Defo need to calculate
-- file table size. Then instead of maintaing two pointers, maintain two
-- builders, one for the file table and one for concatenated contents.
