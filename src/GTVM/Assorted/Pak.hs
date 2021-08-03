{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}

module GTVM.Assorted.Pak
  ( Pak(..)
  , PakHeader(..)
  , PakHeaderFTE(..)
  , pPakHeader
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

{-
sFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Bytes
sFlowchart = serialize bFlowchart

bFlowchart :: MonadReader BinaryCfg m => [FlowchartEntryBlock] -> m Builder
bFlowchart = concatM . fmap bFlowchartEntryBlock

bFlowchartEntryBlock :: MonadReader BinaryCfg m => FlowchartEntryBlock -> m Builder
bFlowchartEntryBlock (FlowchartEntryBlock bs1 entries) = bNullPadTo b 2116
  where b = concatM [bBSFixedNullPadded bs1 32, bCount bW32 bFlowchartEntry entries]

bFlowchartEntry :: MonadReader BinaryCfg m => FlowchartEntry -> m Builder
bFlowchartEntry (FlowchartEntry u1 u2 bs1 bs2) =
    concatM [bW32 u1, bW32 u2, bBSFixedNullPadded bs1 64, bBSFixedNullPadded bs2 32]

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
-}
