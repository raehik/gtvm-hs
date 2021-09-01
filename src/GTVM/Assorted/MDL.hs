{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module GTVM.Assorted.MDL where

import           GTVM.Common.Binary
import           GTVM.Common.Binary.Parse
import           Text.Megaparsec
import qualified Data.ByteString as BS
import           Data.Void
import           Data.Word
import           Control.Monad.Reader

import           GHC.Generics   ( Generic )

import           GTVM.Common.Binary.Util -- TODO TMP

type Bytes = BS.ByteString

data MDL = MDL MDLHeader [MDLSegment]
    deriving (Eq, Show, Generic)

data MDLHeader = MDLHeader
  { mdlHeaderX1        :: Word32
  , mdlHeaderX2        :: Word32
  , mdlHeaderGeoOffset :: Word32
  , mdlHeaderGeoSize   :: Word32
  , mdlHeaderGeoName   :: Bytes
  , mdlHeaderMotOffset :: Word32
  , mdlHeaderMotSize   :: Word32
  , mdlHeaderMotName   :: Bytes
  } deriving (Eq, Show, Generic)

data MDLSegment
  = MDLSegMDHD Bytes
  | MDLSegMDST Bytes Bytes Bytes Bytes Bytes Bytes Bytes Bytes Bytes Bytes
  -- ^ TODO: Apparently a *list* of strings? But no indication of list length??
  -- I've seen 16-18. Bastards.
  | MDLSegMDST' Bytes
  | MDLSegMDMT Bytes
  | MDLSegMDOB Bytes
  | MDLSegMDMS Bytes
  | MDLSegMDTP Bytes
  | MDLSegMODH Bytes
  | MDLSegMOCL Bytes
  | MDLSegMOST Bytes
    deriving (Eq, Show, Generic)

pMDL
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m MDL
pMDL = do
    header <- pMDLHeader
    return $ MDL header []

pMDLHeader
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m MDLHeader
pMDLHeader = MDLHeader <$> pW32 <*> pW32 <*> pW32 <*> pW32 <*> pBSTextFixed 0x18 <*> pW32 <*> pW32 <*> pBSTextFixed 0x18

pMDLSegments
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m [MDLSegment]
pMDLSegments = many pMDLSegment <* eof

pMDLSegment
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m MDLSegment
pMDLSegment = pBS' 4 >>= \case
  "MDHD" -> MDLSegMDHD <$> pW32Raw
  "MDST" -> MDLSegMDST' <$> pW32Raw
  "MDOB" -> MDLSegMDOB <$> pW32Raw
  "MDMT" -> MDLSegMDMT <$> pW32Raw
  "MDMS" -> MDLSegMDMS <$> pW32Raw
  "MDTP" -> MDLSegMDTP <$> pW32Raw
  "MOHD" -> MDLSegMODH <$> pW32Raw
  "MOCL" -> MDLSegMOCL <$> pW32Raw
  "MOST" -> MDLSegMOST <$> pW32Raw
  _      -> customFailure undefined
  where
    pW32Raw = pW32 >>= pBS'

tmpParse
    :: MonadIO m => ParsecT Void Bytes (Reader BinaryCfg) a -> FilePath -> m (Either String a)
tmpParse p fp = runParserBinFile p fp binCfgSCP

pMDL'
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m MDL
pMDL' = do
    x1 <- pMDLHeader
    x2 <- pMDLSegment
    x3 <- pMDLSegment
    return $ MDL x1 [x2,x3]

pMDLSegments'
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m [MDLSegment]
pMDLSegments' = many pMDLSegment
