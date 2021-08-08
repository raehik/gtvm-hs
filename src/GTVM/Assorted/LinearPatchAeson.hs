-- | I don't like how Aeson and co. enforce their operation through typeclasses.
--   This is my solution: throw it all in herew.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module GTVM.Assorted.LinearPatchAeson () where

import           LinearPatch
import           LinearPatch.Patch
import           LinearPatch.Text

import qualified Data.Aeson as Aeson
import           Data.Aeson ( ToJSON, FromJSON )
import           GTVM.Common.Orphans()

jsonCfgTextPatch :: Aeson.Options
jsonCfgTextPatch = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
  , Aeson.rejectUnknownFields = True }

instance ToJSON   TextPatch where
    toJSON     = Aeson.genericToJSON     jsonCfgTextPatch
    toEncoding = Aeson.genericToEncoding jsonCfgTextPatch
instance FromJSON TextPatch where
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

jsonCfgReplaceMany :: Aeson.Options
jsonCfgReplaceMany = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
  , Aeson.rejectUnknownFields = True }

instance ToJSON   ReplaceMany where
    toJSON     = Aeson.genericToJSON     jsonCfgReplaceMany
    toEncoding = Aeson.genericToEncoding jsonCfgReplaceMany
instance FromJSON ReplaceMany where
    parseJSON  = Aeson.genericParseJSON  jsonCfgReplaceMany

jsonCfgOffset :: Aeson.Options
jsonCfgOffset = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 6
  , Aeson.rejectUnknownFields = True }

instance ToJSON   Offset where
    toJSON     = Aeson.genericToJSON     jsonCfgOffset
    toEncoding = Aeson.genericToEncoding jsonCfgOffset
instance FromJSON Offset where
    parseJSON  = Aeson.genericParseJSON  jsonCfgOffset

jsonCfgReplacementMeta :: Aeson.Options
jsonCfgReplacementMeta = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2
  , Aeson.rejectUnknownFields = True }

instance ToJSON   ReplacementMeta where
    toJSON     = Aeson.genericToJSON     jsonCfgReplacementMeta
    toEncoding = Aeson.genericToEncoding jsonCfgReplacementMeta
instance FromJSON ReplacementMeta where
    parseJSON  = Aeson.genericParseJSON  jsonCfgReplacementMeta
