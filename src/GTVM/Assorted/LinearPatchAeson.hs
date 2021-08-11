-- | I don't like how Aeson and co. enforce their operation through typeclasses.
--   This is my solution: throw it all in herew.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE StandaloneDeriving   #-}

module GTVM.Assorted.LinearPatchAeson () where

import           LinearPatch
import           LinearPatch.Patch
import           LinearPatch.Text

import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.Aeson.Combinators.Encode as ACE
import           Data.Aeson
import qualified Data.Aeson.Types              as Aeson
import           Data.Aeson.Encoding
--import           GTVM.Common.Orphans()
import           Text.Megaparsec
import           HexByteString
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Builder        as BB
import           Data.Void
import qualified Data.Text                      as Text

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

instance ToJSON   BS.ByteString where
    toJSON     = hexByteStringJSONEncode
    toEncoding = unsafeToEncoding . prettyHexByteString'

instance FromJSON BS.ByteString where
    parseJSON  = hexByteStringJSONDecode

newtype HexByteString = HexByteString { unHexByteString :: BS.ByteString }

instance Show HexByteString where
    show = Text.unpack . prettyHexByteString . unHexByteString

--instance ToJSON   HexByteString where
--    toJSON     = hexByteStringJSONEncode . unHexByteString
--    toEncoding = unsafeToEncoding . prettyHexByteString' . unHexByteString

--instance FromJSON HexByteString where
--    parseJSON  = fmap HexByteString . hexByteStringJSONDecode

deriving via BS.ByteString instance ToJSON   HexByteString
deriving via BS.ByteString instance FromJSON HexByteString

hexByteStringJSONEncode :: BS.ByteString -> Value
hexByteStringJSONEncode = String . prettyHexByteString

hexByteStringJSONDecode :: Value -> Aeson.Parser BS.ByteString
hexByteStringJSONDecode = withText "hex bytestring" $ \o ->
    case parseMaybe @Void pHexByteString o of
      Nothing -> fail "failed to parse hex bytestring (TODO)"
      Just o' -> pure o'

{-
replaceManyJSONEncode :: ReplaceMany -> Value
replaceManyJSONEncode o = object
    [ "bytes"        .= rsBytes       o
    , "expected_len" .= rsExpectedLen o
    , "offsets"      .= rsOffsets     o ]

replaceManyJSONDecode :: ACD.Decoder ReplaceMany
replaceManyJSONDecode = ReplaceMany
    <$> ACD.key "bytes" ACD.auto
    <*> ACD.nullable (ACD.key "expected_len" ACD.auto)
    <*> ACD.key "offsets" ACD.auto
-}

{-
instance ToJSON   ReplacementMeta where
    toJSON     = replacementMetaJSONEncode
    toEncoding = ACE.toEncoding $ ACE.Encoder $ replacementMetaJSONEncode
instance FromJSON ReplacementMeta where
    parseJSON  = f where ACD.Decoder f = replacementMetaJSONDecode

replacementMetaJSONEncode :: ReplacementMeta -> Value
replacementMetaJSONEncode o = object
    [ "null_terminates" .= rmNullTerminates o
    , "expected"        .= rmExpected       o ]

replacementMetaJSONDecode :: ACD.Decoder ReplacementMeta
replacementMetaJSONDecode = ReplacementMeta
    <$> ACD.key "null_terminates" ACD.auto
    <*> ACD.key "expected"        ACD.auto
-}
