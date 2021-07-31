{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}

module GTVM.Assorted.Flowchart where

import           GTVM.SCP
import           GTVM.Common.Binary
import           GTVM.Common.Parse
import           GTVM.Common.Serialize
import           Text.Megaparsec
import           Data.Void
import           Data.Word
import           Control.Monad.Reader

-- | bytestring is 32 bytes
data FlowchartEntryBlock = FlowchartEntryBlock Bytes [FlowchartEntry]

-- | 1st bytestring is 64 bytes, 2nd is 32 bytes. Null-packed to end.
data FlowchartEntry = FlowchartEntry Word64 Bytes Bytes

parseFlowchartBytes :: Bytes -> Either String [FlowchartEntryBlock]
parseFlowchartBytes = parseFlowchartBytes' "" binCfgSCP

parseFlowchartBytes' :: String -> BinaryCfg -> Bytes -> Either String [FlowchartEntryBlock]
parseFlowchartBytes' = parseBin (many pFlowchartEntryBlock)

pFlowchartEntryBlock
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m FlowchartEntryBlock
pFlowchartEntryBlock = FlowchartEntryBlock <$> pBytestringFixed 32 <*> pCount pW32 pFlowchartEntry

pFlowchartEntry
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m FlowchartEntry
pFlowchartEntry = FlowchartEntry <$> pW64 <*> pBytestringFixed 64 <*> pBytestringFixed 32

sFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Bytes
sFlowchart = serialize bFlowchart

bFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Builder
bFlowchart = concatM . fmap bFlowchartEntryBlock

bFlowchartEntryBlock :: MonadReader BinaryCfg m => FlowchartEntryBlock -> m Builder
bFlowchartEntryBlock (FlowchartEntryBlock bs1 entries) =
    concatM [bBSFixedNullPadded 32 bs1, bCount bW32 bFlowchartEntry entries]

bFlowchartEntry :: MonadReader BinaryCfg m => FlowchartEntry -> m Builder
bFlowchartEntry (FlowchartEntry w64 bs1 bs2) =
    concatM [bW64 w64, bBSFixedNullPadded 64 bs1, bBSFixedNullPadded 32 bs2]
