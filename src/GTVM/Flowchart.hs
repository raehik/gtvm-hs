{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}

module GTVM.Flowchart where

import Binrep
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.ByteString ( Rep(..) )
import Binrep.Type.Text qualified
import Binrep.Type.Int
import Binrep.Predicate.NullPad

import Refined
import Refined.Class
import Refined.Extra

import Data.Aeson qualified as Aeson
import Data.Aeson
import GHC.Generics
import Data.Typeable
import Data.Text ( Text )
import Data.ByteString qualified as BS
import GTVM.Common.Json

type Bytes = BS.ByteString

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
  , entryName   :: WithRefine v (NullPad 64) a
  , entryScript :: WithRefine v (NullPad 32) a
  } deriving stock (Generic, Typeable, Foldable, Show, Eq)

deriving stock instance Functor     (Entry UV)
deriving stock instance Traversable (Entry UV)

jcEntry :: Aeson.Options
jcEntry = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "entry"

instance ToJSON   a => ToJSON   (Entry  v a) where
    toJSON     = genericToJSON     jcEntry
    toEncoding = genericToEncoding jcEntry
instance FromJSON a => FromJSON (Entry UV a) where
    parseJSON  = genericParseJSON  jcEntry

instance                    BLen (Entry V a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Entry V a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Entry V a) where get  = getGeneric  brcNoSum

deriving anyclass instance BLen a =>   Refine (Entry UV a) (Entry  V a)
deriving anyclass instance           Unrefine (Entry  V a) (Entry UV a)

data Block (v :: Validation) a = Block
  { blockName    :: WithRefine v (NullPad 32) a
  , blockEntries :: WithRefine v ('Pascal 'I4 'LE) [Entry v a]
  } deriving stock (Generic, Typeable, Foldable, Show, Eq)

deriving stock instance Functor     (Block UV)
deriving stock instance Traversable (Block UV)

jcBlock :: Aeson.Options
jcBlock = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "block"

instance ToJSON   a => ToJSON   (Block v            a) where
    toJSON     = genericToJSON     jcBlock
    toEncoding = genericToEncoding jcBlock
instance FromJSON a => FromJSON (Block UV a) where
    parseJSON  = genericParseJSON  jcBlock

instance                    BLen (Block V a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Block V a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Block V a) where get  = getGeneric  brcNoSum

deriving anyclass instance (BLen a, Typeable a) =>   Refine (Block UV a) (Block  V a)
deriving anyclass instance                         Unrefine (Block  V a) (Block UV a)

type Flowchart (v :: Validation) a = [WithRefine v (NullPad 2116) (Block v a)]

refineFlowchart
    :: (BLen a, Typeable a)
    => Flowchart UV a
    -> Either RefineException (Flowchart V a)
refineFlowchart = traverse $ refineWithInner refine'

unrefineFlowchart :: Flowchart V a -> Flowchart UV a
unrefineFlowchart = fmap $ unrefineWithInner unrefine'

-- Validate that bytestrings fit in their representation (e.g. well-sized for
-- Pascal-style).
fcBytesRefine
    :: forall (rep :: Binrep.Type.ByteString.Rep)
    .  Predicate rep Bytes
    => Flowchart UV Bytes
    -> Either RefineException (Flowchart UV (Refined rep Bytes))
fcBytesRefine = fcTraverse refine

-- Convert validated bytestrings to the given text encoding.
fcBytesToText
    :: forall enc (rep :: Binrep.Type.ByteString.Rep)
    .  Binrep.Type.Text.Decode enc
    => Flowchart UV (Refined rep Bytes)
    -> Either String (Flowchart UV (Refined enc Text))
fcBytesToText = fcTraverse $ Binrep.Type.Text.decode . unrefine

fcTextToBytes
    :: Binrep.Type.Text.Encode enc
    => Flowchart UV (Refined enc Text)
    -> Flowchart UV Bytes
fcTextToBytes = fcMap Binrep.Type.Text.encode

fcMap :: (a -> b) -> Flowchart UV a -> Flowchart UV b
fcMap = map . fmap . fmap

fcTraverse :: Applicative f => (a -> f b) -> Flowchart UV a -> f (Flowchart UV b)
fcTraverse = traverse . traverse . traverse

findEntryViaScript
    :: Eq a
    => a -> Flowchart UV a -> Maybe (Entry UV a)
findEntryViaScript script fc =
    case matches of
      [match] -> Just match
      _       -> Nothing
  where
    matches = filter predicate $ flowchartEntries fc
    predicate e = withoutRefine (entryScript e) == script

-- Convenience function because unwrapping refinements is kinda annoying.
flowchartEntries :: Flowchart UV a -> [Entry UV a]
flowchartEntries = concat . map (withoutRefine . blockEntries . withoutRefine)
