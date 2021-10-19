{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP.Raehik where

import           Data.Word
import           GHC.Generics
import           Data.Aeson
import           Data.Text      (Text)

import           GTVM.SCP

ex05 = SCPSeg05Textbox' 5 5 "hi from banri" "" 0
ex05' = SCPSeg05Textbox ex05
exXT = SCPXText' "hi from banri" SpeakerBanri Nothing

data SCPX bs
  = SCPXPrimitive     (SCPSegment bs)
  | SCPXPrimitiveList [SCPSegment bs] -- ^ (speedup because this is common)
  | SCPXText SCPXText
    deriving (Eq, Show, Generic)

exT = [SCPXPrimitive ex05', SCPXText exXT, SCPXPrimitive ex05']

jcTest :: Options
jcTest = defaultOptions
  { constructorTagModifier = camelTo2 '_' . drop 4
  , sumEncoding = defaultTaggedObject
    { tagFieldName = "command"
    , contentsFieldName = "arguments" }}

instance ToJSON   a => ToJSON   (SCPX a) where
    toJSON     = genericToJSON     jcTest
    toEncoding = genericToEncoding jcTest
instance FromJSON a => FromJSON (SCPX a) where
    parseJSON  = genericParseJSON  jcTest

