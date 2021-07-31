{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module GTVM.Assorted.Flowchart where

import           GTVM.SCP
import           GTVM.Common.Binary
import           GTVM.Common.Binary.Parse
import           GTVM.Common.Binary.Serialize
import           GTVM.Common.Binary.Util
import           Text.Megaparsec
import qualified Data.ByteString as BS
import           Data.Void
import           Data.Word
import           Control.Monad.Reader

-- | bytestring is 32 bytes
data FlowchartEntryBlock = FlowchartEntryBlock Bytes [FlowchartEntry] deriving (Eq, Show)

-- | 1st bytestring is 64 bytes, 2nd is 32 bytes. Null-packed to end.
data FlowchartEntry = FlowchartEntry Word64 Bytes Bytes deriving (Eq, Show)

{-
parseFlowchartBytes :: Bytes -> Either String [FlowchartEntryBlock]
parseFlowchartBytes = parseFlowchartBytes' "" binCfgSCP

parseFlowchartBytes' :: String -> BinaryCfg -> Bytes -> Either String [FlowchartEntryBlock]
parseFlowchartBytes' = _
-}

fcTest :: MonadReader BinaryCfg m => Bytes -> m Bytes
fcTest bs = runParserBin pFlowchart bs >>= sFlowchart . fromRight

fcTestFile :: (MonadIO m) => FilePath -> BinaryCfg -> m Bytes
fcTestFile fp = runReaderT $ do
    bs <- liftIO $ BS.readFile fp
    fcTest bs

fcTestFile' :: (MonadIO m) => FilePath -> BinaryCfg -> m Bool
fcTestFile' fp = runReaderT $ do
    bs <- liftIO $ BS.readFile fp
    bs' <- fcTest bs
    return $ bs == bs'

fromRight :: Either a b -> b
fromRight (Left  _) = error "no"
fromRight (Right b) = b

pFlowchart
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m [FlowchartEntryBlock]
pFlowchart = many pFlowchartEntryBlock <* eof

pFlowchartEntryBlock
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m FlowchartEntryBlock
pFlowchartEntryBlock = pNullPadTo p 2116
  where p = FlowchartEntryBlock <$> pCStringPad 32 <*> pCount pW32 pFlowchartEntry

pFlowchartEntry
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m FlowchartEntry
pFlowchartEntry = FlowchartEntry <$> pW64 <*> pCStringPad 64 <*> pCStringPad 32

sFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Bytes
sFlowchart = serialize bFlowchart

bFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Builder
bFlowchart = concatM . fmap bFlowchartEntryBlock

bFlowchartEntryBlock :: MonadReader BinaryCfg m => FlowchartEntryBlock -> m Builder
bFlowchartEntryBlock (FlowchartEntryBlock bs1 entries) = bNullPadTo b 2116
  where b = concatM [bBSFixedNullPadded bs1 32, bCount bW32 bFlowchartEntry entries]

bFlowchartEntry :: MonadReader BinaryCfg m => FlowchartEntry -> m Builder
bFlowchartEntry (FlowchartEntry w64 bs1 bs2) =
    concatM [bW64 w64, bBSFixedNullPadded bs1 64, bBSFixedNullPadded bs2 32]
