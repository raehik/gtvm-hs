{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Silly stuff to get Aeson playing nice.
module BinaryPatch.JSON where

import           Data.Aeson
import           HexByteString
import           Text.Megaparsec
import           Data.Void
import           BinaryPatch.Pretty

instance FromJSON HexByteString where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void pHexByteString t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure (HexByteString t')
instance ToJSON   HexByteString where
    toJSON = String . prettyHexByteString . unHexByteString

jsonCfgCamelDrop :: Int -> Options
jsonCfgCamelDrop x = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop x
  , rejectUnknownFields = True }

instance ToJSON   a => ToJSON   (MultiPatches a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 3)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 3)
instance FromJSON a => FromJSON (MultiPatches a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 3)

instance ToJSON   a => ToJSON   (MultiPatch a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 2)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 2)
instance FromJSON a => FromJSON (MultiPatch a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 2)

instance ToJSON   a => ToJSON   (Offset a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 1)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 1)
instance FromJSON a => FromJSON (Offset a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 1)

instance ToJSON   a => ToJSON   (OffsetMeta a) where
    toJSON     = genericToJSON     (jsonCfgCamelDrop 2)
    toEncoding = genericToEncoding (jsonCfgCamelDrop 2)
instance FromJSON a => FromJSON (OffsetMeta a) where
    parseJSON  = genericParseJSON  (jsonCfgCamelDrop 2)
