{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TODO: Likely delete this. In general, I should be only be JSON-codecing
--   with specifically 'HexByteString' (a newtype wrapper), or 'ByteString's
--   should be UNSAFELY converted to 'Text's.
--
-- It's already a lot better, since I'm using hex bytestrings. But the user
-- experience is bad, because a lot of the time if you're trying to JSON a
-- 'ByteString', you're doing it because actually you think it's UTF-8. I want
-- to put barriers in front of that.
--
-- Oh, I'm now only using this in flowcharts. And the "lexed" version of them I
-- don't really want to support anyway. Almost there then!

module GTVM.Common.JSONByteStringOrphanToDelete where

import           Data.Aeson
import qualified Data.ByteString    as BS

import           HexByteString
import           Text.Megaparsec
import           Data.Void

instance FromJSON BS.ByteString where
    parseJSON = withText "hex bytestring" $ \t ->
        case parseMaybe @Void pHexByteString t of
          Nothing -> fail "failed to parse hex bytestring (TODO)"
          Just t' -> pure t'
instance ToJSON   BS.ByteString where
    toJSON = String . prettyHexByteString
