module GTVM.Flowchart where

import Binrep
import Binrep.Types.Common ( Endianness(..) )
import Binrep.Types.ByteString ( Rep(..) )
import Binrep.Types.Text qualified
import Binrep.Types.Int
import Binrep.Predicates.NullPadTo
import Refined
import Refined.WithRefine
import Data.Aeson qualified as Aeson
import Data.Aeson
import GHC.Generics ( Generic )
import Data.Typeable
import Data.Text ( Text )
import Data.ByteString qualified as BS
import GTVM.Common.Json

type Bytes = BS.ByteString

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

instance BinRep EntryType where
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

data Entry (ps :: PredicateStatus) a = Entry
  { entryIndex  :: I 'U 'I4 'LE
  , entryType   :: EntryType
  , entryName   :: WithRefine ps (NullPadTo 64) a
  , entryScript :: WithRefine ps (NullPadTo 32) a
  } deriving stock (Generic, Typeable, Show, Foldable, Eq)

deriving stock instance Functor     (Entry 'Unenforced)
deriving stock instance Traversable (Entry 'Unenforced)

jcEntry :: Aeson.Options
jcEntry = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "entry"

instance ToJSON   a => ToJSON   (Entry ps          a) where
    toJSON     = genericToJSON     jcEntry
    toEncoding = genericToEncoding jcEntry
instance FromJSON a => FromJSON (Entry 'Unenforced a) where
    parseJSON  = genericParseJSON  jcEntry

instance forall (rep :: Binrep.Types.ByteString.Rep) bs bs'
    . ( bs' ~ Refined rep bs
      , ByteLen bs'
      ) => ByteLen (Entry 'Enforced bs') where
    blen e =   blen (entryIndex e)
             + blen (entryType e)
             + blen (entryName e)
             + blen (entryScript e)

instance forall (rep :: Binrep.Types.ByteString.Rep) bs bs'
    . ( bs' ~ Refined rep bs
      , BinRep bs'
      , ByteLen bs'
      ) => BinRep (Entry 'Enforced bs') where
    fromBin = Entry <$> fromBin <*> fromBin <*> fromBin <*> fromBin
    toBin e = do toBin $ entryIndex  e
                 toBin $ entryType   e
                 toBin $ entryName   e
                 toBin $ entryScript e

unenforceEntry :: Entry 'Enforced bs -> Entry 'Unenforced bs
unenforceEntry ee = ee
  { entryName   = unenforce $ entryName   ee
  , entryScript = unenforce $ entryScript ee }

enforceEntry
    :: ByteLen bs
    => Entry 'Unenforced bs
    -> Either RefineException (Entry 'Enforced bs)
enforceEntry ue = do
    en <- enforce $ entryName   ue
    es <- enforce $ entryScript ue
    return ue { entryName = en, entryScript = es }

data Block (ps :: PredicateStatus) a = Block
  { blockName    :: WithRefine ps (NullPadTo 32) a
  , blockEntries :: WithRefine ps ('Pascal 'I4 'LE) [Entry ps a]
  } deriving stock (Generic, Typeable, Show, Foldable, Eq)

deriving stock instance Functor     (Block 'Unenforced)
deriving stock instance Traversable (Block 'Unenforced)

jcBlock :: Aeson.Options
jcBlock = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "block"

instance ToJSON   a => ToJSON   (Block ps          a) where
    toJSON     = genericToJSON     jcBlock
    toEncoding = genericToEncoding jcBlock
instance FromJSON a => FromJSON (Block 'Unenforced a) where
    parseJSON  = genericParseJSON  jcBlock

instance forall (rep :: Binrep.Types.ByteString.Rep) bs bs'
    . ( bs' ~ Refined rep bs
      , ByteLen bs'
      ) => ByteLen (Block 'Enforced bs') where
    blen b = blen (blockName b) + blen (blockEntries b)

instance forall (rep :: Binrep.Types.ByteString.Rep) bs bs'
    . ( bs' ~ Refined rep bs
      , ByteLen bs', BinRep bs'
      ) => BinRep (Block 'Enforced bs') where
    fromBin = Block <$> fromBin <*> fromBin
    toBin b = do toBin $ blockName    b
                 toBin $ blockEntries b

unenforceBlock :: Block 'Enforced bs -> Block 'Unenforced bs
unenforceBlock eb = eb
  { blockName    = unenforce $ blockName eb
  , blockEntries = withRefine $ fmap unenforceEntry $ withoutRefine $ blockEntries eb }

enforceBlock
    :: (ByteLen bs, Typeable bs)
    => Block 'Unenforced bs
    -> Either RefineException (Block 'Enforced bs)
enforceBlock ub = do
    bn <- enforce             $ blockName    ub
    be <- enforceBlockEntries $ blockEntries ub
    return ub { blockName = bn, blockEntries = be }
  where
    enforceBlockEntries ubes = do
        bes <- traverse enforceEntry $ withoutRefine ubes
        enforce $ withRefine bes

type Flowchart (ps :: PredicateStatus) a
  = [WithRefine ps (NullPadTo 2116) (Block ps a)]

unenforceFlowchart :: Flowchart 'Enforced a -> Flowchart 'Unenforced a
unenforceFlowchart = fmap (withRefine . unenforceBlock . withoutRefine)

enforceFlowchart
    :: forall (rep :: Binrep.Types.ByteString.Rep) bs bs'
    .  ( bs' ~ Refined rep bs
       , ByteLen bs'
       , Typeable bs, Typeable rep
       )
    => Flowchart 'Unenforced bs'
    -> Either RefineException (Flowchart 'Enforced bs')
enforceFlowchart ufc = do
    bs <- traverse (enforceBlock . withoutRefine) ufc
    traverse (enforce . withRefine) bs

-- Validate that bytestrings fit in their representation (e.g. well-sized for
-- Pascal-style).
fcBytesRefine
    :: forall (rep :: Binrep.Types.ByteString.Rep)
    .  Predicate rep Bytes
    => Flowchart 'Unenforced Bytes
    -> Either RefineException (Flowchart 'Unenforced (Refined rep Bytes))
fcBytesRefine = fcTraverse refine

-- Convert validated bytestrings to the given text encoding.
fcBytesToText
    :: forall enc (rep :: Binrep.Types.ByteString.Rep)
    .  Binrep.Types.Text.Decode enc
    => Flowchart 'Unenforced (Refined rep Bytes)
    -> Either String (Flowchart 'Unenforced (Refined enc Text))
fcBytesToText = fcTraverse $ Binrep.Types.Text.decode . unrefine

fcTextToBytes
    :: ( Binrep.Types.Text.Encode enc
       , rep ~ Binrep.Types.ByteString.Rep )
    => Flowchart 'Unenforced (Refined enc Text)
    -> Flowchart 'Unenforced Bytes
fcTextToBytes = fcMap Binrep.Types.Text.encode

fcTraverse
    :: Applicative f
    => (a -> f b)
    -> Flowchart 'Unenforced a
    -> f (Flowchart 'Unenforced b)
fcTraverse f = traverse $ traverse $ traverse f

fcMap
    :: (a -> b)
    -> Flowchart 'Unenforced a
    -> Flowchart 'Unenforced b
fcMap f = fmap $ fmap $ fmap f

findEntryViaScript
    :: Eq a
    => a -> Flowchart 'Unenforced a -> Maybe (Entry 'Unenforced a)
findEntryViaScript script fc =
    case matches of
      [match] -> Just match
      _       -> Nothing
  where
    matches = filter predicate $ flowchartEntries fc
    predicate e = withoutRefine (entryScript e) == script

-- Convenience function because unwrapping refinements is kinda annoying.
flowchartEntries :: Flowchart 'Unenforced a -> [Entry 'Unenforced a]
flowchartEntries = concat . map (withoutRefine . blockEntries . withoutRefine)
