{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module GTVM.SCP.Parse
  ( parseSCPBytes
  , parseSCPBytes'
  , parseSCPFile
  , checkSCPDir
  , pSCP
  , pSCPSeg
  ) where

import           GTVM.SCP
import           GTVM.Common.Binary.Parse
import           GTVM.Common.Binary
import           Text.Megaparsec
import qualified Data.ByteString as BS
import           Data.Void
import           Data.Word
import           Data.List.NonEmpty ( NonEmpty(..) )
import           Control.Monad.Reader
import           Data.Function ( (&) )
import qualified Data.List as List
import qualified System.Directory as Dir

parseSCPBytes :: Bytes -> Either String [SCPSegment]
parseSCPBytes = parseSCPBytes' "" binCfgSCP

parseSCPBytes' :: String -> BinaryCfg -> Bytes -> Either String [SCPSegment]
parseSCPBytes' = parseBin (many pSCPSeg)

parseSCPFile :: MonadIO m => FilePath -> m (Either String [SCPSegment])
parseSCPFile fp = do
    bs <- liftIO $ BS.readFile fp
    return $ parseSCPBytes' fp binCfgSCP bs

checkSCPDir :: MonadIO m => FilePath -> m ()
checkSCPDir dir = do
    files <- liftIO $ Dir.listDirectory dir
    let scpFiles = filter (List.isSuffixOf ".scp") files
    flip mapM_ scpFiles $ \fp -> do
        bs <- liftIO $ BS.readFile (dir <> "/" <> fp)
        case parseSCPBytes' fp binCfgSCP bs of
          Left _ -> liftIO (putStrLn fp)
          Right scp -> do
            let textboxCount = textboxesInSCP scp
                outStr = fp <> "," <> show textboxCount
            liftIO $ putStrLn outStr

textboxesInSCP :: [SCPSegment] -> Int
textboxesInSCP segs = foldr go 0 (map isTextbox segs)
  where
    isTextbox (SCPSeg05Textbox{}) = True
    isTextbox _                   = True
    go True  i = i+1
    go False i = i

--------------------------------------------------------------------------------

pSCP :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m [SCPSegment]
pSCP = many pSCPSeg <* eof

pSCPSeg :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m SCPSegment
pSCPSeg = pW8 >>= \case
  0x00 -> SCPSeg00 & return
  0x01 -> SCPSeg01BG <$> pBS <*> pW8 <*> pW8
  0x02 -> SCPSeg02SFX <$> pBS <*> pW8
  0x03 -> SCPSeg03 <$> pW8 <*> pBS <*> pW8
  0x04 -> SCPSeg04 <$> pW8 <*> pW8
  0x05 -> SCPSeg05Textbox <$> pW8 <*> pW32 <*> pBS <*> pBS <*> pW32
  -- no 0x06
  0x07 -> SCPSeg07SCP <$> pBS
  0x08 -> SCPSeg08 & return
  0x09 -> SCPSeg09 <$> pW8 <*> pCount pW8 pBSW32
  0x0A -> SCPSeg0A <$> pW8 <*> pW8 <*> pW32 <*> pW32 <*> pW32
  0x0B -> SCPSeg0B <$> pW8 <*> pW8
  0x0C -> SCPSeg0CFlag <$> pW8 <*> pW8
  0x0D -> SCPSeg0D <$> pW8
  0x0E -> SCPSeg0E <$> pW8
  0x0F -> SCPSeg0F & return
  0x10 -> SCPSeg10 <$> pW8 <*> pW8 <*> pW8
  0x11 -> SCPSeg11EventCG <$> pBS
  0x12 -> SCPSeg12 & return
  0x13 -> SCPSeg13 <$> pW8 <*> pBS <*> pW8 <*> pW32
  0x14 -> SCPSeg14 <$> pW8
  0x15 -> SCPSeg15 & return
  0x16 -> SCPSeg16Wadai & return
  0x17 -> SCPSeg17 <$> pW8 <*> pW8
  0x18 -> SCPSeg18 <$> pW8 <*> pW8
  0x19 -> SCPSeg19 <$> pW8 <*> pW8
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  0x1D -> SCPSeg1D & return
  0x1E -> SCPSeg1E <$> pW8
  0x1F -> SCPSeg1FDelay & return
  0x20 -> SCPSeg20 <$> pW8
  0x21 -> SCPSeg21 & return
  0x22 -> SCPSeg22 <$> pBS <*> pCount pW8 pBSW32
  0x23 -> SCPSeg23SFX & return
  0x24 -> SCPSeg24 <$> pBS
  0x25 -> SCPSeg25 & return
  0x26 -> SCPSeg26 & return
  0x27 -> SCPSeg27 <$> pBS <*> pW8 <*> pW8
  0x28 -> SCPSeg28 <$> pBS <*> pW8 <*> pW8
  0x29 -> SCPSeg29 <$> pBS <*> pW8 <*> pW8
  0x2A -> SCPSeg2A <$> pW8 <*> pW8
  0x2B -> SCPSeg2B <$> pW8 <*> pW8 <*> pW8 <*> pBS <*> pW8
  0x2C -> SCPSeg2CMap & return
  0x2D -> SCPSeg2D <$> pBS <*> pW8 <*> pW8
  0x2E -> SCPSeg2E <$> pW8 <*> pW8 <*> pW32 <*> pW32
  0x2F -> SCPSeg2F <$> pW8
  0x30 -> SCPSeg30 <$> pW8
  0x31 -> SCPSeg31 & return
  0x32 -> SCPSeg32 <$> pW32 <*> pW8
  0x33 -> SCPSeg33 <$> pW8 <*> pW8
  0x34 -> SCPSeg34 <$> pW32
  0x35 -> SCPSeg35 <$> pBS
  0x36 -> SCPSeg36 <$> pW8 <*> pW32
  0x37 -> SCPSeg37 <$> pW8 <*> pW32
  0x38 -> SCPSeg38 <$> pW8 <*> pW32
  0x39 -> SCPSeg39 & return
  0x3A -> SCPSeg3A <$> pW8 <*> pW8
  0x3B -> SCPSeg3B <$> pW8
  0x3C -> SCPSeg3C <$> pW8 <*> pW8
  0x3D -> SCPSeg3D & return
  0x3E -> SCPSeg3E <$> pW8 <*> pW8
  0x3F -> SCPSeg3F <$> pW8 <*> pW32 <*> pW32
  0x40 -> SCPSeg40 <$> pW8
  0x41 -> SCPSeg41 <$> pW8 <*> pW32 <*> pW32
  0x42 -> SCPSeg42 <$> pW8
  0x43 -> SCPSeg43SFX <$> pBS
  0x44 -> SCPSeg44SFX <$> pBS
  0x45 -> SCPSeg45SFX <$> pBS <*> pW8
  0x46 -> SCPSeg46 <$> pW8 <*> pW8
  0x47 -> SCPSeg47 <$> pW8 <*> pW8
  0x48 -> SCPSeg48 & return
  0x49 -> SCPSeg49 & return
  0x4A -> SCPSeg4A & return
  0x4B -> SCPSeg4B & return
  0x4C -> SCPSeg4C <$> pBS
  0x4D -> SCPSeg4D & return
  0x4E -> SCPSeg4E & return
  0x4F -> SCPSeg4F <$> pBS <*> pW8 <*> pW8
  0x50 -> SCPSeg50 <$> pBS <*> pW8 <*> pW8
  0x51 -> SCPSeg51 <$> pBS <*> pW8 <*> pW8
  0x52 -> SCPSeg52 <$> pBS
  0x53 -> SCPSeg53 <$> pBS <*> pW8 <*> pW8
  0x54 -> SCPSeg54 <$> pW8 <*> pW8
  0x55 -> SCPSeg55 <$> pW8 <*> pW8
  0x56 -> SCPSeg56 <$> pW8 <*> pW8
  0x57 -> SCPSeg57 <$> pW8 <*> pW8
  0x58 -> SCPSeg58 <$> pW8 <*> pW8
  0x59 -> SCPSeg59 <$> pW8
  0x5A -> SCPSeg5A <$> pW32 <*> pW8
  0x5B -> SCPSeg5B <$> pW32
  0x5C -> SCPSeg5C <$> pW8
  0x5D -> SCPSeg5D <$> pW32
  0x5E -> SCPSeg5E <$> pW32
  0x5F -> SCPSeg5F <$> pW32
  0x60 -> SCPSeg60 <$> pW32
  0x61 -> SCPSeg61 <$> pW8 <*> pW8
  0x62 -> SCPSeg62 <$> pW8
  0x63 -> SCPSeg63 & return
  0x64 -> SCPSeg64 & return
  0x65 -> SCPSeg65Trophy & return
  0x66 -> SCPSeg66 & return
  0x67 -> SCPSeg67 & return
  0x68 -> SCPSeg68 & return
  0x69 -> SCPSeg69 & return
  0x6A -> SCPSeg6A <$> pW8
  0x6B -> SCPSeg6B <$> pW8
  0x6C -> SCPSeg6CWipe <$> pW8 <*> pW32 <*> pW32 <*> pW32
  0x6D -> SCPSeg6DWipe <$> pW8 <*> pW32 <*> pW32 <*> pW32
  0x6E -> SCPSeg6E & return
  0x6F -> SCPSeg6F & return
  0x70 -> SCPSeg70 <$> pW8 <*> pW8
  0x71 -> SCPSeg71 <$> pW8 <*> pW8
  0x72 -> SCPSeg72 & return
  0x73 -> SCPSeg73Kyoro <$> pW8 <*> pW32
  0x74 -> SCPSeg74 & return
  0x75 -> SCPSeg75 <$> pW8
  0x76 -> SCPSeg76 & return
  0x77 -> SCPSeg77SCP <$> pW8
  b    -> unexpected (Tokens (b :| []))

--------------------------------------------------------------------------------

-- | Parse a bytestring, followed by a 'Word32'.
pBSW32 :: (MonadParsec e Bytes m, MonadReader BinaryCfg m) => m (Bytes, Word32)
pBSW32 = (,) <$> pBS <*> pW32
