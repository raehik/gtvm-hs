module CSV where

import           Data.Csv
import           Data.Text            ( Text )
import qualified Data.ByteString.UTF8 as BSU8
import           Text.Read            ( readMaybe )
import           GTVM.Assorted.BSReplace

data CSVStrReplace = CSVStrReplace
  { csvSrOffset          :: ReadInt
  , csvSrReplStr         :: Text
  , csvSrAvailableSpace  :: Maybe ReadInt
  , csvSrSucceedingNulls :: Maybe ReadInt
  , csvSrOrigStr         :: Maybe Text
  } deriving (Eq, Show)

newtype ReadInt = ReadInt { unReadInt :: Int } deriving (Eq, Show)
instance FromField ReadInt where
    parseField s =
        case readMaybe (BSU8.toString s) of
          Nothing -> fail "integer read fail"
          Just x  -> return (ReadInt x)

instance FromNamedRecord CSVStrReplace where
    parseNamedRecord m =
        CSVStrReplace
        <$> m .: "Address"
        <*> m .: "Translated"
        <*> m .: "Available space"
        <*> m .: "Succeeding null bytes"
        <*> m .: "String"

csvToStrReplace :: CSVStrReplace -> StrReplace
csvToStrReplace csv = StrReplace
  { srText = csvSrReplStr csv
  , srAddress = (unReadInt . csvSrOffset) csv
  , srMeta = Just $ StrReplaceMeta
      { srmNullTerminates = nullTerminatesAt
      , srmExpected = csvSrOrigStr csv
      , srmMaxLength = availableSpace } }
  where
    availableSpace = unReadInt <$> csvSrAvailableSpace csv
    nullTerminatesAt = do
        as <- availableSpace
        ReadInt succeedingNulls <- csvSrSucceedingNulls csv
        return $ as - succeedingNulls
