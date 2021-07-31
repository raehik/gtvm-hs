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

fromRight :: Either a b -> b
fromRight (Left  _) = error "no"
fromRight (Right b) = b

pFlowchart
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m [FlowchartEntryBlock]
pFlowchart = many pFlowchartEntryBlock <* eof

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
