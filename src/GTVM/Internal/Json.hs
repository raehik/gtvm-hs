module GTVM.Internal.Json
  ( module GTVM.Internal.Json
  , ToJSON(..), genericToEncoding, genericToJSON
  , FromJSON(..), genericParseJSON
  ) where

import Data.Aeson
import Data.Aeson.Types ( Parser )
import GHC.Generics ( Generic, Rep )

-- We cheat and use the same string for both the field label and constructor tag
-- modifier, because we figure that you'll only be using this on a product type
-- or an enum-like sum type (no inner fields).
jcGtvmhs :: String -> Options
jcGtvmhs x = defaultOptions
  { fieldLabelModifier     = labelMod
  , constructorTagModifier = labelMod
  , rejectUnknownFields    = True
  } where labelMod = camelTo2 '_' . drop (length x)

-- | Shortcut for genericParseJSON (gtvm-hs)
gpjg :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
gpjg = genericParseJSON . jcGtvmhs

-- | Shortcut for genericToJSON (gtvm-hs)
gtjg :: (Generic a, GToJSON' Value Zero (Rep a)) => String -> a -> Value
gtjg = genericToJSON . jcGtvmhs

-- | Shortcut for genericToEncoding (gtvm-hs)
gteg :: (Generic a, GToJSON' Encoding Zero (Rep a)) => String -> a -> Encoding
gteg = genericToEncoding . jcGtvmhs
