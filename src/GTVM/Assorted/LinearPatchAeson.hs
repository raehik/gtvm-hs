-- | I don't like how Aeson and co. enforce their operation through typeclasses.
--   This is my solution: throw it all in herew.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications     #-}

module GTVM.Assorted.LinearPatchAeson () where

import           LinearPatch
import           LinearPatch.Patch
import           LinearPatch.Text

import           Data.Aeson
import           Data.Aeson.Encoding
--import           GTVM.Common.Orphans()
import           Text.Megaparsec
import           HexByteString
import qualified Data.ByteString        as BS
import           Data.Void

jsonCfgTextPatch :: Options
jsonCfgTextPatch = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 2
  , rejectUnknownFields = True }

instance ToJSON   TextPatch where
    toJSON     = genericToJSON     jsonCfgTextPatch
    toEncoding = genericToEncoding jsonCfgTextPatch
instance FromJSON TextPatch where
    parseJSON  = genericParseJSON  jsonCfgTextPatch

jsonCfgTextPatchMeta :: Options
jsonCfgTextPatchMeta = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 3
  , rejectUnknownFields = True }

instance ToJSON   TextPatchMeta where
    toJSON     = genericToJSON     jsonCfgTextPatchMeta
    toEncoding = genericToEncoding jsonCfgTextPatchMeta
instance FromJSON TextPatchMeta where
    parseJSON  = genericParseJSON  jsonCfgTextPatchMeta

jsonCfgReplaceMany :: Options
jsonCfgReplaceMany = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 2
  , rejectUnknownFields = True }

instance ToJSON   ReplaceMany where
    toJSON     = genericToJSON     jsonCfgReplaceMany
    toEncoding = genericToEncoding jsonCfgReplaceMany
instance FromJSON ReplaceMany where
    parseJSON  = genericParseJSON  jsonCfgReplaceMany

jsonCfgOffset :: Options
jsonCfgOffset = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 6
  , rejectUnknownFields = True }

instance ToJSON   Offset where
    toJSON     = genericToJSON     jsonCfgOffset
    toEncoding = genericToEncoding jsonCfgOffset
instance FromJSON Offset where
    parseJSON  = genericParseJSON  jsonCfgOffset

jsonCfgReplacementMeta :: Options
jsonCfgReplacementMeta = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop 2
  , rejectUnknownFields = True }

instance ToJSON   ReplacementMeta where
    toJSON     = genericToJSON     jsonCfgReplacementMeta
    toEncoding = genericToEncoding jsonCfgReplacementMeta
instance FromJSON ReplacementMeta where
    parseJSON  = genericParseJSON  jsonCfgReplacementMeta

instance ToJSON   BS.ByteString where
    toJSON     = String . prettyHexByteString
    toEncoding = unsafeToEncoding . prettyHexByteString'

instance FromJSON BS.ByteString where
    parseJSON  = withText "hex bytestring" $ \v ->
        case parseMaybe @Void pHexByteString v of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just v' -> pure v'
