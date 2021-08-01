module GTVM.Common.Orphans where

import           Data.Aeson
import qualified Data.ByteString    as BS
import qualified Data.Text.Encoding as Text

-- lol Aeson you dicks
instance ToJSON BS.ByteString where
    toJSON = String . Text.decodeUtf8
instance FromJSON BS.ByteString where
    parseJSON = withText "ByteString" $ pure . Text.encodeUtf8
