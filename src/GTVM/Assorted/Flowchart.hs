{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

module GTVM.Assorted.Flowchart
  ( FlowchartEntryBlock
  , FlowchartEntry
  , FlowchartEntryBlock'
  , FlowchartEntry'
  , pFlowchart
  , sFlowchart
  , fcToAltFc
  , altFcToFc
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
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import           Control.Monad.State

import           Data.Aeson
import           GHC.Generics

type Bytes = BS.ByteString

-- | bytestring is 32 bytes
data FlowchartEntryBlock = FlowchartEntryBlock Bytes [FlowchartEntry] deriving (Eq, Show, Generic)

-- | 1st bytestring is 64 bytes, 2nd is 32 bytes. Null-packed to end.
data FlowchartEntry = FlowchartEntry Word32 Word32 Bytes Bytes deriving (Eq, Show, Generic)

pFlowchart
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m [FlowchartEntryBlock]
pFlowchart = many pFlowchartEntryBlock <* eof

pFlowchartEntryBlock
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m FlowchartEntryBlock
pFlowchartEntryBlock = pNullPadTo p 2116
  where p = FlowchartEntryBlock <$> pBSTextFixed 32 <*> pCount pW32 pFlowchartEntry

pFlowchartEntry
    :: (MonadParsec Void Bytes m, MonadReader BinaryCfg m) => m FlowchartEntry
pFlowchartEntry =
    FlowchartEntry <$> pW32 <*> pW32 <*> pBSTextFixed 64 <*> pBSTextFixed 32

sFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Bytes
sFlowchart = serialize bFlowchart

bFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Builder
bFlowchart = concatM . fmap bFlowchartEntryBlock

bFlowchartEntryBlock :: MonadReader BinaryCfg m => FlowchartEntryBlock -> m Builder
bFlowchartEntryBlock (FlowchartEntryBlock bs1 entries) = bNullPadTo b 2116
  where b = concatM [bBSNullPadTo bs1 32, bCount bW32 bFlowchartEntry entries]

bFlowchartEntry :: MonadReader BinaryCfg m => FlowchartEntry -> m Builder
bFlowchartEntry (FlowchartEntry u1 u2 bs1 bs2) =
    concatM [bW32 u1, bW32 u2, bBSNullPadTo bs1 64, bBSNullPadTo bs2 32]

--------------------------------------------------------------------------------

data FlowchartEntryBlock' = FlowchartEntryBlock' Text [FlowchartEntry'] deriving (Eq, Show, Generic)
data FlowchartEntry' = FlowchartEntry' Word32 Text Text deriving (Eq, Show, Generic)

instance ToJSON FlowchartEntryBlock'
instance ToJSON FlowchartEntry'
instance FromJSON FlowchartEntryBlock'
instance FromJSON FlowchartEntry'

fcToAltFc :: [FlowchartEntryBlock] -> [FlowchartEntryBlock']
fcToAltFc = fmap fcebToAltFceb

fcebToAltFceb :: FlowchartEntryBlock -> FlowchartEntryBlock'
fcebToAltFceb (FlowchartEntryBlock dateBS entries) =
    FlowchartEntryBlock' (Text.decodeUtf8 dateBS) (fmap fceToAltFce entries)

fceToAltFce :: FlowchartEntry -> FlowchartEntry'
fceToAltFce (FlowchartEntry _ u1 textBS scriptBS) =
    FlowchartEntry' u1 (Text.decodeUtf8 textBS) (extractScriptFilepath (Text.decodeUtf8 scriptBS))

extractScriptFilepath :: Text -> Text
extractScriptFilepath = Text.drop 7 . Text.dropEnd 4

altFcToFc :: [FlowchartEntryBlock'] -> [FlowchartEntryBlock]
altFcToFc x = flip evalState 0 $ sequence (altFcebToFceb <$> x)

altFcebToFceb :: MonadState Int m => FlowchartEntryBlock' -> m FlowchartEntryBlock
altFcebToFceb (FlowchartEntryBlock' date entries) = do
    entries' <- sequence (altFceToFce <$> entries)
    return $ FlowchartEntryBlock (Text.encodeUtf8 date) entries'

-- TODO: ensure that c is Word64-bound
altFceToFce :: MonadState Int m => FlowchartEntry' -> m FlowchartEntry
altFceToFce (FlowchartEntry' u1 text script) = do
    c <- get
    modify (+1)
    return $ FlowchartEntry (fromIntegral c) u1 (Text.encodeUtf8 text) (Text.encodeUtf8 ("script/" <> script <> ".scp"))

--------------------------------------------------------------------------------

instance ToJSON FlowchartEntryBlock
instance ToJSON FlowchartEntry
instance FromJSON FlowchartEntryBlock
instance FromJSON FlowchartEntry
