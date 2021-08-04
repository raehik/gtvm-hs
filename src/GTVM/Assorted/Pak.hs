-- | Not implemented: it looks like the game has built-in SL01 deserializing for
--   paks. They should be separate in this tool, but I could provide some handy
--   dandy connectors ("compress and archive", "dearchive and decompress").

{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}

module GTVM.Assorted.Pak
  ( Pak(..)
  , PakHeader(..)
  , PakHeaderFTE(..)
  , pPakHeader
  , sPak
  , nextMultiple
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
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)

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
bPak (Pak unk files) = do
    (bFT, bFs) <- evalStateT (go files) (mempty, mempty, totalFTSize)
    bHeader <- concatM [bW32 (fromIntegral (length files)), bW32 unk, pure bFT]
    bHeaderPadded <- bNullPadTo (pure bHeader) totalFTSize
    return $ bHeaderPadded <> bFs
  where
    -- Guessing that the 0x18 null pad between the file table and contents is
    -- for 0x20 blocking. At any rate, it reserializes the original to the byte,
    -- and appears to work for repacking with no issues.
    totalFTSize = nextMultiple blockTo (0x08 + (length files * 0x20))
    blockTo = 0x20
    go = \case
      [] -> do
        (bFT, bFs, _) <- get
        return (bFT, bFs)
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

nextMultiple :: Integral a => a -> a -> a
nextMultiple a b = b + (a - (b `mod` a))
