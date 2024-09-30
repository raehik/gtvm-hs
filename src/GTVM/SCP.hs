{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-} -- for g-d-f workaround (search `$(pure [])`)
{-# LANGUAGE UndecidableInstances #-} -- for Symparsec

module GTVM.SCP where

import GTVM.Internal.Json

import GHC.Generics ( Generic )
import Data.Typeable ( Typeable )

import Data.Aeson qualified as Aeson
import Binrep
import Binrep.Util.ByteOrder
import Binrep.Common.Via.Generically.NonSum
import Binrep.Type.Prefix.Count ( CountPrefixed )

import Strongweak
import Strongweak.Generic

import Symparsec.Parsers qualified as Symparsec
import Symparsec.Run     qualified as Symparsec
import Generic.Data.MetaParse.Cstr -- TODO what to import
import GHC.TypeLits ( KnownNat, natVal' )

import Optics

import Numeric.Natural ( Natural )
import Data.Text ( Text )
import Data.Word ( Word8, Word32 )

type W8  = Word8
type W32 = ByteOrdered LittleEndian Word32
type PfxLenW8 = CountPrefixed Word8 []

newtype AW32Pairs s a = AW32Pairs
  { unAW32Pairs :: SW s (PfxLenW8 (a, SW s W32)) }
    deriving stock Generic

deriving stock instance Show a => Show (AW32Pairs 'Weak a)
deriving stock instance Eq   a => Eq   (AW32Pairs 'Weak a)

deriving stock instance Show a => Show (AW32Pairs 'Strong a)
deriving stock instance Eq   a => Eq   (AW32Pairs 'Strong a)

instance Functor (AW32Pairs 'Weak) where
    fmap f = AW32Pairs . map (over _1 f) . unAW32Pairs

instance Foldable (AW32Pairs 'Weak) where
    foldMap f = mconcat . map (f . fst) . unAW32Pairs

instance Traversable (AW32Pairs 'Weak) where
    traverse f (AW32Pairs xs) = do
        xs' <- traverse (traverseFst f) xs
        return $ AW32Pairs xs'

traverseFst :: Applicative f => (a -> f b) -> (a, x) -> f (b, x)
traverseFst f (a, x) = do
    b <- f a
    return (b, x)

instance Weaken (AW32Pairs 'Strong a) where
    type Weak   (AW32Pairs 'Strong a) = AW32Pairs 'Weak a
    weaken (AW32Pairs x) = AW32Pairs $ map (\(l, r) -> (l, weaken r)) $ weaken x

instance (Typeable a, Show a) => Strengthen (AW32Pairs 'Strong a) where
    strengthen (AW32Pairs a) = do
        case traverse go a of
          Left  e -> Left e
          Right b -> do
            case strengthen b of
              Left  e -> Left e
              Right c -> Right $ AW32Pairs c
      where
        go (l, r) = do
            case strengthen r of
              Left  e  -> Left e
              Right r' -> Right (l, r')

deriving via (PfxLenW8 (a, W32)) instance BLen a => BLen (AW32Pairs 'Strong a)
deriving via (PfxLenW8 (a, W32)) instance Put  a => Put  (AW32Pairs 'Strong a)
deriving via (PfxLenW8 (a, W32)) instance Get  a => Get  (AW32Pairs 'Strong a)

-- TODO wtf do these look like? and shouldn't I do deriving via?
instance ToJSON   a => ToJSON   (AW32Pairs 'Weak a)
instance FromJSON a => FromJSON (AW32Pairs 'Weak a)

newtype W322Block (s :: Strength) = W322Block
    { unW322Block :: SW s (PfxLenW8 (SW s (PfxLenW8 (SW s W32)))) }
    deriving (Generic)

deriving stock instance Show (W322Block 'Weak)
deriving stock instance Eq   (W322Block 'Weak)

deriving stock instance Show (W322Block 'Strong)
deriving stock instance Eq   (W322Block 'Strong)

deriving via (PfxLenW8 (PfxLenW8 W32)) instance BLen (W322Block 'Strong)
deriving via (PfxLenW8 (PfxLenW8 W32)) instance Put  (W322Block 'Strong)
deriving via (PfxLenW8 (PfxLenW8 W32)) instance Get  (W322Block 'Strong)

deriving via [[Natural]] instance ToJSON   (W322Block 'Weak)
deriving via [[Natural]] instance FromJSON (W322Block 'Weak)

instance Weaken (W322Block 'Strong) where
    type Weak   (W322Block 'Strong) = W322Block 'Weak
    weaken (W322Block a) = W322Block $ map (map weaken . weaken) $ weaken a

instance Strengthen (W322Block 'Strong) where
    strengthen (W322Block a) = do
        case traverse (traverse strengthen) a of -- strengthen ints
          Left  e -> Left e
          Right b -> do
            case traverse strengthen b of -- strengthen inner lists
              Left e -> Left e
              Right c  -> do
                case strengthen c of -- strengthen outer list
                  Left e -> Left e
                  Right d -> Right $ W322Block d

data Seg05Text (s :: Strength) a = Seg05Text
  { seg05TextSpeakerUnkCharID :: SW s W8
  , seg05TextSpeakerID :: SW s W32
  , seg05TextText :: a
  , seg05TextVoiceLine :: a
  , seg05TextCounter :: SW s W32
  } deriving stock (Generic)

deriving stock instance Show a => Show (Seg05Text 'Weak a)
deriving stock instance Eq   a => Eq   (Seg05Text 'Weak a)
deriving stock instance Functor     (Seg05Text 'Weak)
deriving stock instance Foldable    (Seg05Text 'Weak)
deriving stock instance Traversable (Seg05Text 'Weak)

deriving stock instance Show a => Show (Seg05Text 'Strong a)
deriving stock instance Eq   a => Eq   (Seg05Text 'Strong a)

deriving via (GenericallyNonSum (Seg05Text Strong a))
    instance BLen a => BLen (Seg05Text Strong a)
deriving via (GenericallyNonSum (Seg05Text Strong a))
    instance  Put a =>  Put (Seg05Text Strong a)
deriving via (GenericallyNonSum (Seg05Text Strong a))
    instance  Get a =>  Get (Seg05Text Strong a)

instance ToJSON   a => ToJSON   (Seg05Text 'Weak a) where
    toJSON     = gtjg "seg05Text"
    toEncoding = gteg "seg05Text"
instance FromJSON a => FromJSON (Seg05Text 'Weak a) where
    parseJSON  = gpjg "seg05Text"

instance Weaken (Seg05Text 'Strong a) where
    type Weak   (Seg05Text 'Strong a) = Seg05Text 'Weak a
    weaken = weakenGeneric
instance Strengthen (Seg05Text 'Strong a) where
    strengthen = strengthenGeneric

data Seg (s :: Strength) a
  = Seg00
  | Seg01BG a (SW s W8) (SW s W8)
  | Seg02SFX a (SW s W8)
  | Seg03 (SW s W8) a (SW s W8)
  | Seg04 (SW s W8) (SW s W8)

  | Seg05 (Seg05Text s a)
  -- ^ Includes player-facing text.

  -- no 0x06

  | Seg07SCP a
  | Seg08

  | Seg09Choice (SW s W8) (AW32Pairs s a)
  -- ^ Includes player-facing text. Choice selection. The 'W32's appear to be
  --   the same counter as textboxes!
  --
  -- The 'W8' seems to be an file-unique identifier for the choice selection.
  -- SCP files with multiple choices have 0, 1, 2 etc. in ascending order.

  | Seg0A (W322Block s)

  | Seg0B (SW s W8) (SW s W8)
  -- ^ Appears to indicate where a given choice jumps to.
  --
  -- First 'W8' appears to specify which choice selection it relates to.
  -- Second 'W8' appears to be the choice index in that choice selection
  -- (usually 0, 1). They may be highly separated, in cases where a choice
  -- changes lots of dialog.

  | Seg0CFlag (SW s W8) (SW s W8)
  | Seg0D (SW s W8)
  | Seg0E (SW s W8)
  | Seg0F
  | Seg10 (SW s W8) (SW s W8) (SW s W8)
  | Seg11EventCG a
  | Seg12

  | Seg13 (SW s W8) a (SW s W8) (SW s W32)
  -- ^ Possibly player-facing text. Registers words for the feast
  --   Danganronpa-style minigame, but using indices which correspond to
  --   textures with the text on. The usage of the text here is unknown.

  | Seg14 (SW s W8)
  | Seg15
  | Seg16Wadai
  | Seg17 (SW s W8) (SW s W8)
  | Seg18 (SW s W8) (SW s W8)
  | Seg19 (SW s W8) (SW s W8)
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  | Seg1D
  | Seg1E (SW s W8)
  | Seg1FDelay
  | Seg20 (SW s W8)
  | Seg21

  | Seg22 a (AW32Pairs s a)
  -- ^ Includes player-facing text. Choice, plus an extra string. Seem to be
  --   used in the conversation events.

  | Seg23SFX

  | Seg24 a
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg25
  | Seg26

  | Seg27 a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg28 a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg29 a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg2A (SW s W8) (SW s W8)

  | Seg2B (SW s W8) (SW s W8) (SW s W8) a (W322Block s)

  | Seg2CMap

  | Seg2D a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg2E (SW s W8) (SW s W8) (SW s W32) (SW s W32)
  | Seg2F (SW s W8)
  | Seg30 (SW s W8)
  | Seg31
  | Seg32 (SW s W32) (SW s W8)
  | Seg33 (SW s W8) (SW s W8)
  | Seg34 (SW s W32)

  | Seg35 a
  -- ^ Likely player-facing. Kinda sounds like it's a type of Banri choice.

  | Seg36 (SW s W8) (SW s W32)
  | Seg37 (SW s W8) (SW s W32)
  | Seg38 (SW s W8) (SW s W32)
  | Seg39
  | Seg3A (SW s W8) (SW s W8)
  | Seg3B (SW s W8)
  | Seg3C (SW s W8) (SW s W8)
  | Seg3D
  | Seg3E (SW s W8) (SW s W8)
  | Seg3F (SW s W8) (SW s W32) (SW s W32)
  | Seg40 (SW s W8)
  | Seg41 (SW s W8) (SW s W32) (SW s W32)
  | Seg42 (SW s W8)

  -- these don't appear very SFXy in code, but did in data
  | Seg43SFX a
  | Seg44SFX a
  | Seg45SFX a (SW s W8)

  | Seg46 (SW s W8) (SW s W8)
  | Seg47 (SW s W8) (SW s W8)
  | Seg48
  | Seg49
  | Seg4A
  | Seg4B

  | Seg4C a
  -- Unknown. Appears unused.

  | Seg4D
  | Seg4E

  | Seg4F a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg50 a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg51 a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg52 a
  -- ^ Text not user-facing. Refers to a @sound/voice@ file - all the uses I've
  --   seen are girls telling you 飲んで飲んで！

  | Seg53 a (SW s W8) (SW s W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg54 (SW s W8) (SW s W8)
  | Seg55 (SW s W8) (SW s W8)
  | Seg56 (SW s W8) (SW s W8)
  | Seg57 (SW s W8) (SW s W8)
  | Seg58 (SW s W8) (SW s W8)
  | Seg59 (SW s W8)
  | Seg5A (SW s W32) (SW s W8)
  | Seg5B (SW s W32)
  | Seg5C (SW s W8)
  | Seg5D (SW s W32)
  | Seg5E (SW s W32)
  | Seg5F (SW s W32)
  | Seg60 (SW s W32)
  | Seg61 (SW s W8) (SW s W8)
  | Seg62 (SW s W8) -- 0x01 <= w8 <= 0x11, no default
  | Seg63
  | Seg64
  | Seg65Trophy
  | Seg66
  | Seg67
  | Seg68
  | Seg69
  | Seg6A (SW s W8)
  | Seg6B (SW s W8)
  | Seg6CWipe (SW s W8) (SW s W32) (SW s W32) (SW s W32)
  | Seg6DWipe (SW s W8) (SW s W32) (SW s W32) (SW s W32)
  | Seg6E
  | Seg6F
  | Seg70 (SW s W8) (SW s W8)
  | Seg71 (SW s W8) (SW s W8)
  | Seg72
  | Seg73Kyoro (SW s W8) (SW s W32) -- 0x01 <= w8 <= 0x06, with default
  | Seg74
  | Seg75 (SW s W8)
  | Seg76
  | Seg77SCP (SW s W8)
    deriving stock (Generic)

deriving stock instance Show a => Show (Seg 'Weak a)
deriving stock instance Eq   a => Eq   (Seg 'Weak a)

deriving stock instance Show a => Show (Seg 'Strong a)
deriving stock instance Eq   a => Eq   (Seg 'Strong a)

-- parse SegXX
type ParseSeg =
                   Symparsec.Literal "Seg"
    Symparsec.:*>: Symparsec.Isolate 2 Symparsec.NatHex

-- REMEMBER THIS HAS TO GO ABOVE BINREP
instance CstrParser' Seg where
    type CstrParseResult Seg = Natural
$(pure [])
instance CstrParser  Seg where
    type ReifyCstrParseResult Seg n = KnownNat n
    type ParseCstr Seg cstr =
        Symparsec.Run'_ ParseSeg cstr

instance BLen a => BLen (Seg 'Strong a) where
    blen = blenGenericSum @Seg (\_p -> 1)
instance Put  a => Put  (Seg 'Strong a) where
     put =  putGenericSum @Seg (\p -> put @Word8 (fromIntegral (natVal' p)))
instance Get  a => Get  (Seg 'Strong a) where
     get =  getGenericSum @Seg @Word8 (\p -> fromIntegral (natVal' p)) (==)

jcSeg :: Aeson.Options
jcSeg = Aeson.defaultOptions
  { Aeson.constructorTagModifier  = take 2 . drop (length ("Seg" :: String))
  , Aeson.rejectUnknownFields = True
  , Aeson.sumEncoding = Aeson.TaggedObject
    { Aeson.tagFieldName      = "command_byte"
    , Aeson.contentsFieldName = "arguments"
    }
  }

instance ToJSON   a => ToJSON   (Seg 'Weak a) where
    toJSON     = genericToJSON     jcSeg
    toEncoding = genericToEncoding jcSeg
instance FromJSON a => FromJSON (Seg 'Weak a) where
    parseJSON  = genericParseJSON  jcSeg

type SCP s a = [Seg s a]
--type SCPBin  = SCP 'Strong (Refined NullTerminated B.ByteString)
--type SCPText = SCP 'Weak (AsText 'UTF8)

scpFmap :: (a -> b) -> SCP 'Weak a -> SCP 'Weak b
scpFmap = fmap . fmap

scpTraverse :: Applicative f => (a -> f b) -> SCP 'Weak a -> f (SCP 'Weak b)
scpTraverse = traverse . traverse

scpSegFieldOrdering :: Text -> Text -> Ordering
scpSegFieldOrdering = go
  where
    go "command_byte" _ = LT
    go _ "command_byte" = GT
    go k1 k2 = compare k1 k2

deriving stock instance Functor     (Seg 'Weak)
deriving stock instance Foldable    (Seg 'Weak)
deriving stock instance Traversable (Seg 'Weak)

instance Weaken (Seg 'Strong a) where
    type Weak   (Seg 'Strong a) = Seg 'Weak a
    weaken = weakenGeneric
instance (Typeable a, Show a) => Strengthen (Seg 'Strong a) where
    strengthen = strengthenGeneric
