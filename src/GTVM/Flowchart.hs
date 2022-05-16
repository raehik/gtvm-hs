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

import Refined hiding ( Weaken, weaken, strengthen )
import Raehik.Validate

import Data.Aeson qualified as Aeson
import Data.Aeson
import GHC.Generics
import Data.Typeable
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
  { entryIndex  :: Switch v (I 'U 'I4 'LE)
  , entryType   :: EntryType
  , entryName   :: Switch v (NullPadded 64 a)
  , entryScript :: Switch v (NullPadded 32 a)
  } deriving stock (Generic, Typeable)
deriving stock instance Eq   a => Eq   (Entry V  a)
deriving stock instance Show a => Show (Entry V  a)
deriving stock instance Eq   a => Eq   (Entry UV a)
deriving stock instance Show a => Show (Entry UV a)

instance Functor (Entry UV) where
    fmap f (Entry i t n s) = Entry i t (f n) (f s)

instance Foldable (Entry UV) where
    foldMap f (Entry _i _t n s) = f n <> f s

instance Traversable (Entry UV) where
    traverse f (Entry i t n s) = do
        n' <- f n
        s' <- f s
        return $ Entry i t n' s'

instance Weaken (Entry V a) (Entry UV a) where
    weaken (Entry i t n s) = Entry (weaken i) t (weaken n) (weaken s)

instance BLen a => Strengthen (Entry UV a) (Entry V a) where
    strengthen (Entry i t n s) = do
        i' <- strengthen i
        n' <- strengthen n
        s' <- strengthen s
        return $ Entry i' t n' s'

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

instance Functor (Block UV) where
    fmap f (Block n es) = Block (f n) (fmap (fmap f) es)

instance Foldable (Block UV) where
    foldMap f (Block n es) = f n <> foldMap (foldMap f) es

instance Traversable (Block UV) where
    traverse f (Block n es) = do
        n'  <- f n
        es' <- traverse (traverse f) es
        return $ Block n' es'

instance Weaken (Block V a) (Block UV a) where
    weaken (Block n es) = Block (weaken n) (weaken (weaken es))

instance BLen a => Strengthen (Block UV a) (Block V a) where
    strengthen (Block n es) = do
        n'  <- strengthen n
        es'' <- strengthen es
        es' <- strengthen es''
        return $ Block n' es'

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

newtype Flowchart (v :: Validation) a =
    Flowchart { flowchartEntries :: [Switch v (NullPadded 2116 (Block v a))] }
deriving via [NullPadded 2116 (Block V  a)] instance Eq   a => Eq   (Flowchart V  a)
deriving via [NullPadded 2116 (Block V  a)] instance Show a => Show (Flowchart V  a)
deriving via                  [Block UV a]  instance Eq   a => Eq   (Flowchart UV a)
deriving via                  [Block UV a]  instance Show a => Show (Flowchart UV a)
deriving via                  [Block UV a]  instance ToJSON   a => ToJSON   (Flowchart UV a)
deriving via                  [Block UV a]  instance FromJSON a => FromJSON (Flowchart UV a)
deriving via [NullPadded 2116 (Block V  a)] instance                    BLen (Flowchart V a)
deriving via [NullPadded 2116 (Block V  a)] instance (BLen a, Put a) => Put  (Flowchart V a)
deriving via [NullPadded 2116 (Block V  a)] instance (BLen a, Get a) => Get  (Flowchart V a)

instance Functor (Flowchart UV) where
    fmap f (Flowchart es) = Flowchart (fmap (fmap f) es)

instance Foldable (Flowchart UV) where
    foldMap f (Flowchart es) = foldMap (foldMap f) es

instance Traversable (Flowchart UV) where
    traverse f (Flowchart es) = Flowchart <$> traverse (traverse f) es

instance Weaken (Flowchart V a) (Flowchart UV a) where
    weaken (Flowchart es) = Flowchart (weaken (weaken es))

instance BLen a => Strengthen (Flowchart UV a) (Flowchart V a) where
    strengthen (Flowchart es) = do
        es'' <- strengthen es
        es' <- strengthen es''
        return $ Flowchart es'

findEntryViaScript
    :: Eq a
    => a -> Flowchart UV a -> Maybe (Entry UV a)
findEntryViaScript script (Flowchart es) =
    case matches of
      [match] -> Just match
      _       -> Nothing
  where
    matches = filter predicate $ (concat . map blockEntries) es
    predicate e = (entryScript e) == script

fcBytesRefine
    :: forall (rep :: Binrep.Type.ByteString.Rep)
    .  Predicate rep Bytes
    => Flowchart UV Bytes
    -> Either RefineException (Flowchart UV (AsByteString rep))
fcBytesRefine = traverse refine

fcBytesToText
    :: forall enc (rep :: Binrep.Type.ByteString.Rep)
    .  Binrep.Type.Text.Decode enc
    => Flowchart UV (Refined rep Bytes)
    -> Either String (Flowchart UV (AsText enc))
fcBytesToText = traverse $ Binrep.Type.Text.decode . unrefine

fcTextToBytes
    :: Binrep.Type.Text.Encode enc
    => Flowchart UV (AsText enc)
    -> Flowchart UV Bytes
fcTextToBytes = fmap Binrep.Type.Text.encode
