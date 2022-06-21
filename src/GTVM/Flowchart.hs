{-# LANGUAGE RecordWildCards, OverloadedRecordDot, ApplicativeDo #-}

module GTVM.Flowchart where

import Binrep
import Binrep.Generic
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.ByteString ( Rep(..), AsByteString )
import Binrep.Type.Text qualified
import Binrep.Type.Text ( AsText )
import Binrep.Type.Int
import Binrep.Type.LenPfx
import Binrep.Type.NullPadded

import Refined ( Refined, Predicate, refine, unrefine, RefineException )
import Strongweak
import Strongweak.Generic
import Data.Either.Validation

import Data.Aeson qualified as Aeson
import Data.Aeson hiding ( Success )
import GHC.Generics
import Data.Typeable
import Data.ByteString qualified as B
import GTVM.Common.Json

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

data Entry (s :: Strength) a = Entry
  { entryIndex  :: SW s (I 'U 'I4 'LE)
  , entryType   :: EntryType
  , entryName   :: SW s (NullPadded 64 a)
  , entryScript :: SW s (NullPadded 32 a)
  } deriving stock (Generic, Typeable)
deriving stock instance Eq   a => Eq   (Entry 'Strong a)
deriving stock instance Show a => Show (Entry 'Strong a)
deriving stock instance Eq   a => Eq   (Entry 'Weak a)
deriving stock instance Show a => Show (Entry 'Weak a)

instance Functor (Entry 'Weak) where
    fmap f (Entry i t n s) = Entry i t (f n) (f s)

instance Foldable (Entry 'Weak) where
    foldMap f (Entry _i _t n s) = f n <> f s

instance Traversable (Entry 'Weak) where
    traverse f (Entry i t n s) = do
        n' <- f n
        s' <- f s
        return $ Entry i t n' s'

instance Weaken (Entry 'Strong a) where
    type Weak   (Entry 'Strong a) = Entry 'Weak a
    weaken = weakenGeneric
instance (BLen a, Typeable a, Show a) => Strengthen (Entry 'Strong a) where
    strengthen = strengthenGeneric

jcEntry :: Aeson.Options
jcEntry = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "entry"

instance ToJSON   a => ToJSON   (Entry 'Weak a) where
    toJSON     = genericToJSON     jcEntry
    toEncoding = genericToEncoding jcEntry
instance FromJSON a => FromJSON (Entry 'Weak a) where
    parseJSON  = genericParseJSON  jcEntry

instance                    BLen (Entry 'Strong a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Entry 'Strong a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Entry 'Strong a) where get  = getGeneric  brcNoSum

data Block (s :: Strength) a = Block
  { blockName    :: SW s (NullPadded 32 a)
  , blockEntries :: SW s (LenPfx 'I4 'LE (Entry s a))
  } deriving stock (Generic, Typeable)
deriving stock instance Eq   a => Eq   (Block 'Strong a)
deriving stock instance Show a => Show (Block 'Strong a)
deriving stock instance Eq   a => Eq   (Block 'Weak a)
deriving stock instance Show a => Show (Block 'Weak a)

instance Functor (Block 'Weak) where
    fmap f (Block n es) = Block (f n) (fmap (fmap f) es)

instance Foldable (Block 'Weak) where
    foldMap f (Block n es) = f n <> foldMap (foldMap f) es

instance Traversable (Block 'Weak) where
    traverse f (Block n es) = do
        n'  <- f n
        es' <- traverse (traverse f) es
        return $ Block n' es'

instance Weaken (Block 'Strong a) where
    type Weak   (Block 'Strong a) = Block 'Weak a
    weaken (Block n es) = Block (weaken n) (weaken (weaken es))

instance (BLen a, Show a, Typeable a) => Strengthen (Block 'Strong a) where
    strengthen (Block n es) = do
        case strengthen n of
          Failure err -> Failure err
          Success n'  -> do
            case strengthen es of
              Failure err -> Failure err
              Success es' -> do
                case strengthen es' of
                  Failure err  -> Failure err
                  Success es'' -> Success $ Block n' es''

jcBlock :: Aeson.Options
jcBlock = jsonCfgSepUnderscoreDropN $ fromIntegral $ length "block"

instance ToJSON   a => ToJSON   (Block 'Weak a) where
    toJSON     = genericToJSON     jcBlock
    toEncoding = genericToEncoding jcBlock
instance FromJSON a => FromJSON (Block 'Weak a) where
    parseJSON  = genericParseJSON  jcBlock

instance                    BLen (Block 'Strong a) where blen = blenGeneric brcNoSum
instance (BLen a, Put a) => Put  (Block 'Strong a) where put  = putGeneric  brcNoSum
instance (BLen a, Get a) => Get  (Block 'Strong a) where get  = getGeneric  brcNoSum

newtype Flowchart (s :: Strength) a =
    Flowchart { flowchartEntries :: [SW s (NullPadded 2116 (Block s a))] }
deriving via [NullPadded 2116 (Block 'Strong a)] instance Eq   a => Eq   (Flowchart 'Strong a)
deriving via [NullPadded 2116 (Block 'Strong a)] instance Show a => Show (Flowchart 'Strong a)
deriving via                  [Block 'Weak   a]  instance Eq   a => Eq   (Flowchart 'Weak a)
deriving via                  [Block 'Weak   a]  instance Show a => Show (Flowchart 'Weak a)
deriving via                  [Block 'Weak   a]  instance ToJSON   a => ToJSON   (Flowchart 'Weak a)
deriving via                  [Block 'Weak   a]  instance FromJSON a => FromJSON (Flowchart 'Weak a)
deriving via [NullPadded 2116 (Block 'Strong a)] instance                    BLen (Flowchart 'Strong a)
deriving via [NullPadded 2116 (Block 'Strong a)] instance (BLen a, Put a) => Put  (Flowchart 'Strong a)
deriving via [NullPadded 2116 (Block 'Strong a)] instance (BLen a, Get a) => Get  (Flowchart 'Strong a)

instance Functor (Flowchart 'Weak) where
    fmap f (Flowchart es) = Flowchart (fmap (fmap f) es)

instance Foldable (Flowchart 'Weak) where
    foldMap f (Flowchart es) = foldMap (foldMap f) es

instance Traversable (Flowchart 'Weak) where
    traverse f (Flowchart es) = Flowchart <$> traverse (traverse f) es

instance Weaken (Flowchart 'Strong a) where
    type Weak   (Flowchart 'Strong a) = Flowchart 'Weak a
    weaken (Flowchart es) = Flowchart (weaken (weaken es))

instance (BLen a, Typeable a, Show a) => Strengthen (Flowchart 'Strong a) where
    strengthen (Flowchart es) = do
        case strengthen es of
          Failure err -> Failure err
          Success es' -> do
            case strengthen es' of
              Failure err -> Failure err
              Success es'' -> Success $ Flowchart es''

findEntryViaScript
    :: Eq a
    => a -> Flowchart 'Weak a -> Maybe (Entry 'Weak a)
findEntryViaScript script (Flowchart es) =
    case matches of
      [match] -> Just match
      _       -> Nothing
  where
    matches = filter predicate $ (concat . map blockEntries) es
    predicate e = (entryScript e) == script

fcBytesRefine
    :: forall (rep :: Binrep.Type.ByteString.Rep)
    .  Predicate rep B.ByteString
    => Flowchart 'Weak B.ByteString
    -> Either RefineException (Flowchart 'Weak (AsByteString rep))
fcBytesRefine = traverse refine

fcBytesToText
    :: forall enc (rep :: Binrep.Type.ByteString.Rep)
    .  Binrep.Type.Text.Decode enc
    => Flowchart 'Weak (Refined rep B.ByteString)
    -> Either String (Flowchart 'Weak (AsText enc))
fcBytesToText = traverse $ Binrep.Type.Text.decode . unrefine

fcTextToBytes
    :: Binrep.Type.Text.Encode enc
    => Flowchart 'Weak (AsText enc)
    -> Flowchart 'Weak B.ByteString
fcTextToBytes = fmap Binrep.Type.Text.encode
