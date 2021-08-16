{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV
  ( csvToTextReplace
  , csvDecode
  , colNames
  , getColName
  , CSVTextReplace(..)
  ) where

import           BinaryPatch.Pretty

import           Data.Csv
import qualified Data.Text.Encoding     as Text
import           Data.Text              ( Text )
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.UTF8   as BSU8
import           Text.Read              ( readMaybe )
import qualified Data.Vector            as Vector

import           GHC.Generics           ( Generic )
import           Barbies
import           Data.Functor.Identity
import           Data.Functor.Const

data CSVTextReplace f = CSVTextReplace
  { csvSrOffset          :: f ReadInt
  , csvSrReplText         :: f Text
  , csvSrAvailableSpace  :: f (Maybe ReadInt)
  , csvSrSucceedingNulls :: f (Maybe ReadInt)
  , csvSrOrigText         :: f (Maybe Text)
  } deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB)

newtype ReadInt = ReadInt { unReadInt :: Int } deriving (Eq, Show)
instance FromField ReadInt where
    parseField s =
        case readMaybe (BSU8.toString s) of
          Nothing -> fail "integer read fail"
          Just x  -> return (ReadInt x)

instance FromNamedRecord (CSVTextReplace Identity) where
    parseNamedRecord m =
        CSVTextReplace
        <$> m .: fieldName csvSrOffset
        <*> m .: fieldName csvSrReplText
        <*> m .: fieldName csvSrAvailableSpace
        <*> m .: fieldName csvSrSucceedingNulls
        <*> m .: fieldName csvSrOrigText
      where fieldName f = Text.encodeUtf8 (getColName f)

colNames :: CSVTextReplace (Const Text)
colNames = CSVTextReplace
  { csvSrOffset          = "Offset in file"
  , csvSrReplText        = "Translated"
  , csvSrAvailableSpace  = "Available space"
  , csvSrSucceedingNulls = "Succeeding null bytes"
  , csvSrOrigText        = "Text"
  }

getColName :: (CSVTextReplace (Const Text) -> Const a b) -> a
getColName f = getConst (f colNames)

csvToTextReplace :: CSVTextReplace Identity -> MultiPatch Text
csvToTextReplace csv = MultiPatch
  { mpContents = runIdentity (csvSrReplText csv)
  , mpOffsets =
      [ Offset
        { oOffset = (unReadInt . runIdentity . csvSrOffset) csv
        , oMeta = Just $ OffsetMeta
          { omMaxLength = availableSpace
          , omNullTerminates = nullTerminatesAt
          , omExpected = runIdentity (csvSrOrigText csv) }}]}
  where
    availableSpace = unReadInt <$> (runIdentity . csvSrAvailableSpace) csv
    nullTerminatesAt = do
        as <- availableSpace
        ReadInt succeedingNulls <- (runIdentity . csvSrSucceedingNulls) csv
        return $ as - succeedingNulls

csvDecode :: BL.ByteString -> Either String [CSVTextReplace Identity]
csvDecode bs =
    case decodeByName bs of
      Left err  -> Left err
      Right (_, vec) -> Right $ Vector.toList vec
