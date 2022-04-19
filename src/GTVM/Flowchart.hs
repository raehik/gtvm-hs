module GTVM.Flowchart where

import Raehik.Binary.Codec
import Raehik.Binary.ByteLen
import Raehik.Binary.Predicates.NullPadTo
import Raehik.Binary.Types.Ints
import Raehik.Binary.Types.Strings
import Refined
import Refined.WithRefine
import Data.Aeson qualified as Aeson
import Data.Aeson
import GHC.Generics ( Generic )
import Data.Typeable
import Data.Text ( Text )
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import GTVM.Common.Json

type Flowchart (ps :: PredicateStatus) a
  = [WithRefine ps (NullPadTo 2116) (Block ps a)]

-- 2022-01-19: raehik: so my understanding is that this should get simplified to
-- a no-op?
unenforceFlowchart :: Flowchart 'Enforced a -> Flowchart 'Unenforced a
unenforceFlowchart = fmap (withRefine . unenforceBlock . unWithRefine)

-- this is also hard to define without HLE lol. I need to start using that
enforceFlowchart
    :: Flowchart 'Unenforced (Str 'C)
    -> Either RefineException (Flowchart 'Enforced (Str 'C))
enforceFlowchart ufc = do
    bs <- traverse (enforceBlock . unWithRefine) ufc
    traverse (enforce . withRefine) bs

fcTextify
    :: Flowchart 'Unenforced (Str 'C)
    -> Either Text.UnicodeException (Flowchart 'Unenforced Text)
fcTextify = traverse $ traverse $ traverse $ Text.decodeUtf8' . getStr

fcByteify
    :: Flowchart 'Unenforced Text
    -> Flowchart 'Unenforced (Str 'C)
fcByteify = fmap $ fmap $ fmap $ Str . Text.encodeUtf8

data Block (ps :: PredicateStatus) a = Block
  { blockName    :: WithRefine ps (NullPadTo 32) a
  , blockEntries :: WithRefine ps (LenPfx 'I4 'LE) [Entry ps a]
  } deriving stock (Generic, Typeable, Show, Foldable, Eq)

unenforceBlock :: Block 'Enforced a -> Block 'Unenforced a
unenforceBlock eb = eb
  { blockName    = unenforce $ blockName eb
  , blockEntries = withRefine $ fmap unenforceEntry $ unWithRefine $ blockEntries eb }

enforceBlock
    :: Block 'Unenforced (Str 'C)
    -> Either RefineException (Block 'Enforced (Str 'C))
enforceBlock ub = do
    bn <- enforce             $ blockName    ub
    be <- enforceBlockEntries $ blockEntries ub
    return ub { blockName = bn, blockEntries = be }

-- TODO too much algebra...
enforceBlockEntries
    :: WithRefine 'Unenforced (LenPfx 'I4 'LE) [Entry 'Unenforced (Str 'C)]
    -> Either RefineException (WithRefine 'Enforced (LenPfx 'I4 'LE) [Entry 'Enforced (Str 'C)])
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

instance ByteLen (Block 'Enforced (Str 'C)) where
    blen b = blen (blockName b) + blen (blockEntries b)

instance BinaryCodec (Block 'Enforced (Str 'C)) where
    fromBin = Block <$> fromBin <*> fromBin
    toBin b = do toBin $ blockName    b
                 toBin $ blockEntries b

data Entry (ps :: PredicateStatus) a = Entry
  { entryIndex  :: I 'U 'I4 'LE
  , entryType   :: EntryType
  , entryName   :: WithRefine ps (NullPadTo 64) a
  , entryScript :: WithRefine ps (NullPadTo 32) a
  } deriving stock (Generic, Typeable, Show, Foldable, Eq)

unenforceEntry :: Entry 'Enforced a -> Entry 'Unenforced a
unenforceEntry ee = ee
  { entryName   = unenforce $ entryName   ee
  , entryScript = unenforce $ entryScript ee }

enforceEntry
    :: Entry 'Unenforced (Str 'C)
    -> Either RefineException (Entry 'Enforced (Str 'C))
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

instance ByteLen (Entry 'Enforced (Str 'C)) where
    blen e = blen (entryIndex e) + blen (entryType e) + blen (entryName e) + blen (entryScript e)

instance BinaryCodec (Entry 'Enforced (Str 'C)) where
    fromBin = Entry <$> fromBin <*> fromBin <*> fromBin <*> fromBin
    toBin e = do toBin $ entryIndex  e
                 toBin $ entryType   e
                 toBin $ entryName   e
                 toBin $ entryScript e

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

instance ByteLen EntryType where blen = const $ blen @(I 'U 'I4 'LE) undefined

instance BinaryCodec EntryType where
    toBin et = toBin @(I 'U 'I4 'LE) $ case et of
      EntryTypeRegular        -> 0
      EntryTypeMap            -> 1
      EntryTypeUtage          -> 2
      EntryTypeConversation   -> 3
      EntryTypeClassSelection -> 4
    fromBin = fromBin @(I 'U 'I4 'LE) >>= \case
      0 -> return EntryTypeRegular
      1 -> return EntryTypeMap
      2 -> return EntryTypeUtage
      3 -> return EntryTypeConversation
      4 -> return EntryTypeClassSelection
      n -> fail $ "bad entry type: " <> show n <> ", expected 0-4"

findEntryViaScript
    :: Eq a
    => a -> Flowchart 'Unenforced a -> Maybe (Entry 'Unenforced a)
findEntryViaScript script fc =
    case matches of
      [match] -> Just match
      _       -> Nothing
  where
    matches = filter predicate $ flowchartEntries fc
    predicate e = unWithRefine (entryScript e) == script

-- Convenience function because unwrapping refinements is kinda annoying.
flowchartEntries :: Flowchart 'Unenforced a -> [Entry 'Unenforced a]
flowchartEntries = concat . map (unWithRefine . blockEntries . unWithRefine)
