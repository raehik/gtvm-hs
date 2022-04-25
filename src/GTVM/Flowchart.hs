{-# LANGUAGE RecordWildCards, OverloadedRecordDot #-}

module GTVM.Flowchart where

import Binrep
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.ByteString ( Rep(..) )
import Binrep.Type.Text qualified
import Binrep.Type.Int
import Binrep.Predicate.NullPad

import Refined

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

deriving stock instance Functor     (Entry 'Unvalidated)
deriving stock instance Traversable (Entry 'Unvalidated)

jcEntry :: Aeson.Options
jcEntry = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "entry"

instance ToJSON   a => ToJSON   (Entry v            a) where
    toJSON     = genericToJSON     jcEntry
    toEncoding = genericToEncoding jcEntry
instance FromJSON a => FromJSON (Entry 'Unvalidated a) where
    parseJSON  = genericParseJSON  jcEntry

instance                    BLen (Entry 'Validated a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Entry 'Validated a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Entry 'Validated a) where get  = getGeneric  brcNoSum

refineEntry
    :: BLen a
    => Entry 'Unvalidated a
    -> Either RefineException (Entry 'Validated a)
refineEntry e = do
    let entryIndex = e.entryIndex
        entryType  = e.entryType
    entryName   <- refineWith e.entryName
    entryScript <- refineWith e.entryScript
    return Entry{..}

unrefineEntry :: Entry 'Validated a -> Entry 'Unvalidated a
unrefineEntry e = Entry{..}
  where entryIndex  = e.entryIndex
        entryType   = e.entryType
        entryName   = unrefineWith $ e.entryName
        entryScript = unrefineWith $ e.entryScript

data Block (v :: Validation) a = Block
  { blockName    :: WithRefine v (NullPad 32) a
  , blockEntries :: WithRefine v ('Pascal 'I4 'LE) [Entry v a]
  } deriving stock (Generic, Typeable, Foldable, Show, Eq)

deriving stock instance Functor     (Block 'Unvalidated)
deriving stock instance Traversable (Block 'Unvalidated)

jcBlock :: Aeson.Options
jcBlock = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "block"

instance ToJSON   a => ToJSON   (Block v            a) where
    toJSON     = genericToJSON     jcBlock
    toEncoding = genericToEncoding jcBlock
instance FromJSON a => FromJSON (Block 'Unvalidated a) where
    parseJSON  = genericParseJSON  jcBlock

instance                    BLen (Block 'Validated a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Block 'Validated a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Block 'Validated a) where get  = getGeneric  brcNoSum

refineBlock :: (BLen a, Typeable a) => Block 'Unvalidated a -> Either RefineException (Block 'Validated a)
refineBlock b = do
    blockName    <- refineWith b.blockName
    blockEntries <- refineWithInner (traverse refineEntry) $ b.blockEntries
    return Block{..}

unrefineBlock :: Block 'Validated a -> Block 'Unvalidated a
unrefineBlock b = Block{..}
    where blockName    = unrefineWith $ b.blockName
          blockEntries = unrefineWithInner (fmap unrefineEntry) $ b.blockEntries

type Flowchart (v :: Validation) a = [WithRefine v (NullPad 2116) (Block v a)]

refineFlowchart
    :: (BLen a, Typeable a)
    => Flowchart 'Unvalidated a
    -> Either RefineException (Flowchart 'Validated a)
refineFlowchart = traverse $ refineWithInner refineBlock

unrefineFlowchart :: Flowchart 'Validated a -> Flowchart 'Unvalidated a
unrefineFlowchart = fmap $ unrefineWithInner unrefineBlock

-- Validate that bytestrings fit in their representation (e.g. well-sized for
-- Pascal-style).
fcBytesRefine
    :: forall (rep :: Binrep.Type.ByteString.Rep)
    .  Predicate rep Bytes
    => Flowchart 'Unvalidated Bytes
    -> Either RefineException (Flowchart 'Unvalidated (Refined rep Bytes))
fcBytesRefine = fcTraverse refine

-- Convert validated bytestrings to the given text encoding.
fcBytesToText
    :: forall enc (rep :: Binrep.Type.ByteString.Rep)
    .  Binrep.Type.Text.Decode enc
    => Flowchart 'Unvalidated (Refined rep Bytes)
    -> Either String (Flowchart 'Unvalidated (Refined enc Text))
fcBytesToText = fcTraverse $ Binrep.Type.Text.decode . unrefine

fcTextToBytes
    :: Binrep.Type.Text.Encode enc
    => Flowchart 'Unvalidated (Refined enc Text)
    -> Flowchart 'Unvalidated Bytes
fcTextToBytes = fcMap Binrep.Type.Text.encode

fcMap :: (a -> b) -> Flowchart 'Unvalidated a -> Flowchart 'Unvalidated b
fcMap = map . fmap . fmap

fcTraverse :: Applicative f => (a -> f b) -> Flowchart 'Unvalidated a -> f (Flowchart 'Unvalidated b)
fcTraverse = traverse . traverse . traverse

findEntryViaScript
    :: Eq a
    => a -> Flowchart 'Unvalidated a -> Maybe (Entry 'Unvalidated a)
findEntryViaScript script fc =
    case matches of
      [match] -> Just match
      _       -> Nothing
  where
    matches = filter predicate $ flowchartEntries fc
    predicate e = withoutRefine (entryScript e) == script

-- Convenience function because unwrapping refinements is kinda annoying.
flowchartEntries :: Flowchart 'Unvalidated a -> [Entry 'Unvalidated a]
flowchartEntries = concat . map (withoutRefine . blockEntries . withoutRefine)
