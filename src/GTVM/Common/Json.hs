module GTVM.Common.Json where

import Data.Aeson
import Numeric.Natural

jsonCfgSepUnderscoreDropN :: Natural -> Options
jsonCfgSepUnderscoreDropN n = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (fromIntegral n)
  , rejectUnknownFields = True }
