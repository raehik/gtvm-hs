{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV
  ( csvToStrReplace
  , csvDecode
  , colNames
  , getColName
  , CSVStrReplace(..)
  ) where

import           LinearPatch.Text

import           Data.Csv
import qualified Data.Text.Encoding   as Text
import           Data.Text            ( Text )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BSU8
import           Text.Read            ( readMaybe )
import qualified Data.Vector          as Vector

import           GHC.Generics         ( Generic )
import           Barbies
import           Data.Functor.Identity
import           Data.Functor.Const

data CSVStrReplace f = CSVStrReplace
  { csvSrOffset          :: f ReadInt
  , csvSrReplStr         :: f Text
  , csvSrAvailableSpace  :: f (Maybe ReadInt)
  , csvSrSucceedingNulls :: f (Maybe ReadInt)
  , csvSrOrigStr         :: f (Maybe Text)
  } deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

newtype ReadInt = ReadInt { unReadInt :: Int } deriving (Eq, Show)
instance FromField ReadInt where
    parseField s =
        case readMaybe (BSU8.toString s) of
          Nothing -> fail "integer read fail"
          Just x  -> return (ReadInt x)

instance FromNamedRecord (CSVStrReplace Identity) where
    parseNamedRecord m =
        CSVStrReplace
        <$> m .: fieldName csvSrOffset
        <*> m .: fieldName csvSrReplStr
        <*> m .: fieldName csvSrAvailableSpace
        <*> m .: fieldName csvSrSucceedingNulls
        <*> m .: fieldName csvSrOrigStr
      where fieldName f = Text.encodeUtf8 (getColName f)

colNames :: CSVStrReplace (Const Text)
colNames = CSVStrReplace
  { csvSrOffset          = "Address"
  , csvSrReplStr         = "Translated"
  , csvSrAvailableSpace  = "Available space"
  , csvSrSucceedingNulls = "Succeeding null bytes"
  , csvSrOrigStr         = "String"
  }

getColName :: (CSVStrReplace (Const Text) -> Const a b) -> a
getColName f = getConst (f colNames)

csvToStrReplace :: CSVStrReplace Identity -> TextPatch
csvToStrReplace csv = TextPatch
  { tpText = runIdentity (csvSrReplStr csv)
  , tpAddress = (unReadInt . runIdentity . csvSrOffset) csv
  , tpMeta = Just $ TextPatchMeta
      { tpmNullTerminates = nullTerminatesAt
      , tpmExpected = runIdentity (csvSrOrigStr csv)
      , tpmMaxLength = availableSpace } }
  where
    availableSpace = unReadInt <$> (runIdentity . csvSrAvailableSpace) csv
    nullTerminatesAt = do
        as <- availableSpace
        ReadInt succeedingNulls <- (runIdentity . csvSrSucceedingNulls) csv
        return $ as - succeedingNulls

csvDecode :: BL.ByteString -> Either String [CSVStrReplace Identity]
csvDecode bs =
    case decodeByName bs of
      Left err  -> Left err
      Right (_, vec) -> Right $ Vector.toList vec
