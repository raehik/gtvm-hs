{- | flow_chart.bin code.

Each entry block is a static 2116 bytes. The statically-sized parts making up a
block are:

  * 32 bytes: block name
  *  4 bytes: number of entries in entry list

That leaves us with 2116 - 32 - 4 = 2080 bytes remaining. An astute reader will
recognize 2080 - 32 = 2048. Now it *feels* like the entry list should be coerced
to 2048 bytes. But where do the remaining 32 bytes go?

Whatever. There might be a 32-byte bytestring following the entry list. But I've
confirmed that those bytes are always null. So the entry list gets a 2080
bytesize.

As a side note, this stuff feels like it should be an automatic serialization
from a library or compiler, but the game mentions 0x844 == 2116 directly, and
there's bits that feel hand-rolled (e.g. newline handling for entry names is
done inline). Strange.

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module GTVM.Flowchart where

import GHC.Generics ( Generic )
import Data.Word
import Raehik.Binary
import Refined
import Refined.WithRefine
import Data.Aeson qualified as Aeson
import Data.Aeson
import Data.Typeable
import GTVM.Common.Json
import Data.Serialize
import Data.Text ( Text )
import Data.Text.Encoding.Error qualified as Text
import Data.Text.Encoding qualified as Text

type Flowchart (ps :: PredicateStatus) a
  = List (WithRefine ps (NullPadded 2116) (Block ps a))

-- 2022-01-19: raehik: so my understanding is that this should get simplified to
-- a no-op?
unenforceFlowchart :: Flowchart 'Enforced a -> Flowchart 'Unenforced a
unenforceFlowchart =
    List . fmap (withRefine . unenforceBlock . unWithRefine) . getList

-- this is also hard to define without HLE lol. I need to start using that
enforceFlowchart
    :: Flowchart 'Unenforced CString
    -> Either RefineException (Flowchart 'Enforced CString)
enforceFlowchart ufc = do
    bs <- traverse (enforceBlock . unWithRefine) $ getList ufc
    bs' <- traverse (enforce . withRefine) bs
    return $ List bs'

fcTextify
    :: Flowchart 'Unenforced CString
    -> Either Text.UnicodeException (Flowchart 'Unenforced Text)
fcTextify = traverse $ traverse $ traverse $ Text.decodeUtf8' . getCString

fcByteify :: Flowchart 'Unenforced Text -> Flowchart 'Unenforced CString
fcByteify = fmap $ fmap $ fmap $ CString . Text.encodeUtf8

data Block (ps :: PredicateStatus) a = Block
  { blockName    :: WithRefine ps (NullPadded 32) a
  , blockEntries :: WithRefine ps (LengthPrefixed (W 'LE Word32)) [Entry ps a]
  } deriving stock (Generic, Typeable, Show, Foldable, Eq)

unenforceBlock :: Block 'Enforced a -> Block 'Unenforced a
unenforceBlock eb = eb
  { blockName    = unenforce $ blockName eb
  , blockEntries = withRefine $ fmap unenforceEntry $ unWithRefine $ blockEntries eb }

enforceBlock
    :: Block 'Unenforced CString
    -> Either RefineException (Block 'Enforced CString)
enforceBlock ub = do
    bn <- enforce             $ blockName    ub
    be <- enforceBlockEntries $ blockEntries ub
    return ub { blockName = bn, blockEntries = be }

-- TODO too much algebra...
enforceBlockEntries
    :: WithRefine 'Unenforced (LengthPrefixed (W 'LE Word32)) [Entry 'Unenforced CString]
    -> Either RefineException (WithRefine 'Enforced (LengthPrefixed (W 'LE Word32)) [Entry 'Enforced CString])
enforceBlockEntries ubes = do
    x <- traverse enforceEntry $ unWithRefine ubes
    enforce $ withRefine x

deriving stock instance Functor     (Block 'Unenforced)
deriving stock instance Traversable (Block 'Unenforced)

jcBlock :: Aeson.Options
jcBlock = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "block"

instance ToJSON   a => ToJSON   (Block ps          a) where
    toJSON     = genericToJSON     jcBlock
    toEncoding = genericToEncoding jcBlock
instance FromJSON a => FromJSON (Block 'Unenforced a) where
    parseJSON  = genericParseJSON  jcBlock

instance ByteLength (Block 'Enforced CString) where
    blen b = blen (blockName b) + blen (blockEntries b)

instance Serialize (Block 'Enforced CString) where
    get = Block <$> get <*> get
    put b = do
        put $ blockName    b
        put $ blockEntries b

data Entry (ps :: PredicateStatus) a = Entry
  { entryIndex  :: W 'LE Word32
  , entryType   :: EntryType
  , entryName   :: WithRefine ps (NullPadded 64) a
  , entryScript :: WithRefine ps (NullPadded 32) a
  } deriving stock (Generic, Typeable, Show, Foldable, Eq)

unenforceEntry :: Entry 'Enforced a -> Entry 'Unenforced a
unenforceEntry ee = ee
  { entryName   = unenforce $ entryName   ee
  , entryScript = unenforce $ entryScript ee }

enforceEntry
    :: Entry 'Unenforced CString
    -> Either RefineException (Entry 'Enforced CString)
enforceEntry ue = do
    en <- enforce $ entryName   ue
    es <- enforce $ entryScript ue
    return ue { entryName = en, entryScript = es }

deriving stock instance Functor     (Entry 'Unenforced)
deriving stock instance Traversable (Entry 'Unenforced)

jcEntry :: Aeson.Options
jcEntry = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "entry"

instance ToJSON   a => ToJSON   (Entry ps          a) where
    toJSON     = genericToJSON     jcEntry
    toEncoding = genericToEncoding jcEntry
instance FromJSON a => FromJSON (Entry 'Unenforced a) where
    parseJSON  = genericParseJSON  jcEntry

instance ByteLength (Entry 'Enforced CString) where
    blen e = blen (entryIndex e) + blen (entryType e) + blen (entryName e) + blen (entryScript e)

instance Serialize (Entry 'Enforced CString) where
    get = Entry <$> get <*> get <*> get <*> get
    put e = do
        put $ entryIndex  e
        put $ entryType   e
        put $ entryName   e
        put $ entryScript e

data EntryType
  = EntryTypeRegular        -- ^ "regular" event, some story script
  | EntryTypeMap            -- ^ MAP
  | EntryTypeUtage          -- ^ 〜の宴 TODO party, dinner
  | EntryTypeConversation   -- ^ 〜と話そう
  | EntryTypeClassSelection -- ^ 履修申請
    deriving (Eq, Show, Generic)

jcEntryType :: Aeson.Options
jcEntryType = Aeson.defaultOptions
    { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop (length "EntryType") }

instance ToJSON   EntryType where
    toJSON     = genericToJSON     jcEntryType
    toEncoding = genericToEncoding jcEntryType
instance FromJSON EntryType where
    parseJSON  = genericParseJSON  jcEntryType

instance ByteLength EntryType where blen = const $ blen @(W 'LE Word32) undefined

instance Serialize EntryType where
    put et = put @(W 'LE Word32) $ case et of
      EntryTypeRegular        -> 0
      EntryTypeMap            -> 1
      EntryTypeUtage          -> 2
      EntryTypeConversation   -> 3
      EntryTypeClassSelection -> 4
    get = get @(W 'LE Word32) >>= \case
      0 -> return EntryTypeRegular
      1 -> return EntryTypeMap
      2 -> return EntryTypeUtage
      3 -> return EntryTypeConversation
      4 -> return EntryTypeClassSelection
      n -> fail $ "bad entry type: " <> show n <> ", expected 0-4"
