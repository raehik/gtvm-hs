module LinearPatch.Text where

import           LinearPatch
import           LinearPatch.Patch

import qualified Data.ByteString         as BS
import qualified Data.Text.Encoding      as Text
import           Data.Text               ( Text )

import           GHC.Generics            ( Generic )

data TextPatch = TextPatch
  { tpText    :: Text
  , tpAddress :: Int
  , tpMeta    :: Maybe TextPatchMeta
  } deriving (Eq, Show, Generic)

data TextPatchMeta = TextPatchMeta
  { tpmNullTerminates :: Maybe Int
  , tpmExpected       :: Maybe Text
  , tpmMaxLength      :: Maybe Int
  } deriving (Eq, Show, Generic)

textPatchToBin :: TextPatch -> ReplaceMany
textPatchToBin tp = rs
  where
    -- TODO sucks I gotta do a snoc here >:(
    textToCString t = BS.snoc (Text.encodeUtf8 t) 0x00
    rs = ReplaceMany
        { rsBytes       = textToCString (tpText tp)
        , rsExpectedLen = Nothing
        , rsOffsets     = [offset] }
    offset = Offset
        { offsetAddress   = tpAddress tp
        , offsetMaxLength = maxLen
        , offsetMeta      = meta }
    (meta, maxLen) =
        case tpMeta tp of
          Nothing    -> (Nothing, Nothing)
          Just sMeta ->
            let rm = Just $ ReplacementMeta
                        { rmNullTerminates = tpmNullTerminates sMeta
                        , rmExpected       = textToCString <$> tpmExpected sMeta }
                ml = tpmMaxLength sMeta
            in (rm, ml)

--------------------------------------------------------------------------------

{-
jsonCfgTextPatch :: Aeson.Options
jsonCfgTextPatch = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
  , Aeson.rejectUnknownFields = True }

instance ToJSON   TextReplace where
    toJSON     = Aeson.genericToJSON     jsonCfgTextPatch
    toEncoding = Aeson.genericToEncoding jsonCfgTextPatch
instance FromJSON TextReplace where
    parseJSON  = Aeson.genericParseJSON  jsonCfgTextPatch

jsonCfgTextPatchMeta :: Aeson.Options
jsonCfgTextPatchMeta = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
  , Aeson.rejectUnknownFields = True }

instance ToJSON   TextPatchMeta where
    toJSON     = Aeson.genericToJSON     jsonCfgTextPatchMeta
    toEncoding = Aeson.genericToEncoding jsonCfgTextPatchMeta
instance FromJSON TextPatchMeta where
    parseJSON  = Aeson.genericParseJSON  jsonCfgTextPatchMeta
-}
