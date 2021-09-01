module GTVM.SCP.Util where

import           GTVM.SCP
import qualified Data.ByteString         as BS
import qualified Data.Text.Encoding      as Text
import           Data.Text               (Text)

type Bytes = BS.ByteString

scpTextify :: [SCPSegment Bytes] -> [Either (SCPSegment Bytes) (SCPSegment Text)]
scpTextify = scpConvert decodeUtf8
  where
    decodeUtf8 bs =
        case Text.decodeUtf8' bs of
          Left _  -> Nothing
          Right t -> Just t

scpConvert :: (a -> Maybe b) -> [SCPSegment a] -> [Either (SCPSegment a) (SCPSegment b)]
scpConvert f = map go
  where
    go seg =
        case traverse f seg of
          Just seg' -> Right seg'
          Nothing   -> Left  seg
