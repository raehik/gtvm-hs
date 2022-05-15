{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}

module GTVM.Flowchart where

import Binrep
import Binrep.Generic
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.ByteString ( Rep(..) )
import Binrep.Type.Text qualified
import Binrep.Type.Int
import Binrep.Type.LenPfx
import Binrep.Type.NullPadded

import Refined
import Raehik.Validate

import Data.Aeson qualified as Aeson
import Data.Aeson
import GHC.Generics
import Data.Typeable
import Data.Text ( Text )
import Data.ByteString qualified as BS
import GTVM.Common.Json

type Bytes = BS.ByteString
type UV = 'Unvalidated
type V  = 'Validated

brcNoSum :: Cfg (I 'U 'I4 'LE) -- typevar doesn't matter, not used
brcNoSum = Cfg { cSumTag = undefined }

data EntryType
  = EntryType0Regular        -- ^ "regular" event, some story script
  | EntryType1Map            -- ^ MAP
  | EntryType2Utage          -- ^ 〜の宴 TODO party, dinner
  | EntryType3Conversation   -- ^ 〜と話そう
  | EntryType4ClassSelection -- ^ 履修申請
    deriving stock (Generic, Show, Eq)

jcEntryType :: Aeson.Options
jcEntryType = Aeson.defaultOptions
    { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop (length "EntryTypeX") }

instance ToJSON   EntryType where
    toJSON     = genericToJSON     jcEntryType
    toEncoding = genericToEncoding jcEntryType
instance FromJSON EntryType where
    parseJSON  = genericParseJSON  jcEntryType

brcEntryType :: Cfg (I 'U 'I4 'LE)
brcEntryType = Cfg
  { cSumTag = cSumTagHex $ take 1 . drop (length "EntryType") }

instance BLen EntryType where blen = blenGeneric brcEntryType
instance Put  EntryType where put  = putGeneric  brcEntryType
instance Get  EntryType where get  = getGeneric  brcEntryType

data Entry (v :: Validation) a = Entry
  { entryIndex  :: I 'U 'I4 'LE
  , entryType   :: EntryType
  , entryName   :: Switch v (NullPadded 64 a)
  , entryScript :: Switch v (NullPadded 32 a)
  } deriving stock (Generic, Typeable)
deriving stock instance Eq   a => Eq   (Entry V  a)
deriving stock instance Show a => Show (Entry V  a)
deriving stock instance Eq   a => Eq   (Entry UV a)
deriving stock instance Show a => Show (Entry UV a)

jcEntry :: Aeson.Options
jcEntry = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "entry"

instance ToJSON   a => ToJSON   (Entry UV a) where
    toJSON     = genericToJSON     jcEntry
    toEncoding = genericToEncoding jcEntry
instance FromJSON a => FromJSON (Entry UV a) where
    parseJSON  = genericParseJSON  jcEntry

instance                    BLen (Entry V a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Entry V a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Entry V a) where get  = getGeneric  brcNoSum

data Block (v :: Validation) a = Block
  { blockName    :: Switch v (NullPadded 32 a)
  , blockEntries :: Switch v (LenPfx 'I4 'LE (Entry v a))
  } deriving stock (Generic, Typeable)
deriving stock instance Eq   a => Eq   (Block V  a)
deriving stock instance Show a => Show (Block V  a)
deriving stock instance Eq   a => Eq   (Block UV a)
deriving stock instance Show a => Show (Block UV a)

jcBlock :: Aeson.Options
jcBlock = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "block"

instance ToJSON   a => ToJSON   (Block UV a) where
    toJSON     = genericToJSON     jcBlock
    toEncoding = genericToEncoding jcBlock
instance FromJSON a => FromJSON (Block UV a) where
    parseJSON  = genericParseJSON  jcBlock

instance                    BLen (Block V a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Block V a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Block V a) where get  = getGeneric  brcNoSum

type Flowchart (v :: Validation) a = [Switch v (NullPadded 2116 (Block v a))]

findEntryViaScript
    :: Eq a
    => a -> Flowchart UV a -> Maybe (Entry UV a)
findEntryViaScript script fc =
    case matches of
      [match] -> Just match
      _       -> Nothing
  where
    matches = filter predicate $ (concat . map blockEntries) fc
    predicate e = (entryScript e) == script
