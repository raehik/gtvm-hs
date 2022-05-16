{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module GTVM.SCP where

import GHC.Generics ( Generic )

import Data.Aeson qualified as Aeson
import Data.Aeson ( ToJSON(..), genericToJSON, genericToEncoding, FromJSON(..), genericParseJSON )
import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int ( I(..), ISize(..), ISign(..) )
import Binrep.Type.ByteString ( Rep(..), AsByteString )
import Binrep.Type.Text ( Encoding(..), AsText )
import Binrep.Type.LenPfx
import Data.Vector.Sized qualified as Vector
import Data.Yaml.Pretty qualified as Yaml.Pretty

import Raehik.Validate
import Raehik.Validate.Generic

import Optics

import Numeric.Natural ( Natural )

type Endian = 'LE
type W8  = I 'U 'I1 Endian
type W32 = I 'U 'I4 Endian
type PfxLenW8 a = LenPfx 'I1 Endian a

newtype AW32Pairs v a = AW32Pairs
  { unAW32Pairs :: Switch v (PfxLenW8 (a, Switch v W32)) }
    deriving (Generic)
deriving stock instance Show a => Show (AW32Pairs UV a)
deriving stock instance Eq   a => Eq   (AW32Pairs UV a)

instance Functor (AW32Pairs UV) where
    fmap f = AW32Pairs . map (over _1 f) . unAW32Pairs

instance Foldable (AW32Pairs UV) where
    foldMap f = mconcat . map (f . fst) . unAW32Pairs

instance Traversable (AW32Pairs UV) where
    traverse f (AW32Pairs xs) = do
        xs' <- traverse (traverseFst f) xs
        return $ AW32Pairs xs'

traverseFst :: Applicative f => (a -> f b) -> (a, x) -> f (b, x)
traverseFst f (a, x) = do
    b <- f a
    return (b, x)

instance Weaken (AW32Pairs V a) (AW32Pairs UV a) where
    weaken x =
        case unAW32Pairs x of
          LenPfx x' -> AW32Pairs $ map (\(l, r) -> (l, weaken r)) $ Vector.toList x'

instance Strengthen (AW32Pairs UV a) (AW32Pairs V a) where
    strengthen (AW32Pairs a) = do
        b <- traverse go a
        case lenPfxFromList b of
          Nothing -> Left "TODO nope"
          Just c  -> Right $ AW32Pairs c
      where
        go (l, r) = do
            r' <- strengthen r
            Right (l, r')

deriving via (PfxLenW8 (a, W32)) instance BLen a => BLen (AW32Pairs V a)
deriving via (PfxLenW8 (a, W32)) instance Put  a => Put  (AW32Pairs V a)
deriving via (PfxLenW8 (a, W32)) instance Get  a => Get  (AW32Pairs V a)
instance ToJSON   a => ToJSON   (AW32Pairs UV a)
instance FromJSON a => FromJSON (AW32Pairs UV a)

type UV = 'Unvalidated
type V  = 'Validated
type Sw v a = Switch v a

brcNoSum :: BR.Cfg W8
brcNoSum = BR.Cfg { BR.cSumTag = undefined }

jcProd :: (String -> String) -> Aeson.Options
jcProd f = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . f
  , Aeson.rejectUnknownFields = True
  }

jcSum :: (String -> String) -> (String -> String) -> String -> String -> Aeson.Options
jcSum f g tag contents = (jcProd f)
  { Aeson.constructorTagModifier = g
  , Aeson.sumEncoding = Aeson.TaggedObject
    { Aeson.tagFieldName = tag
    , Aeson.contentsFieldName = contents
    }
  }

newtype W322Block (v :: Validation) = W322Block
    { unW322Block :: Switch v (PfxLenW8 (Switch v (PfxLenW8 (Switch v W32)))) }
    deriving (Generic)
deriving stock instance Show (W322Block UV)
deriving stock instance Eq   (W322Block UV)

deriving via (PfxLenW8 (PfxLenW8 W32)) instance BLen (W322Block V)
deriving via (PfxLenW8 (PfxLenW8 W32)) instance Put  (W322Block V)
deriving via (PfxLenW8 (PfxLenW8 W32)) instance Get  (W322Block V)

deriving via [[Natural]] instance ToJSON   (W322Block UV)
deriving via [[Natural]] instance FromJSON (W322Block UV)

instance Weaken (W322Block V) (W322Block UV) where
    weaken (W322Block a) = W322Block $ map (map weaken . lenPfxToList) $ lenPfxToList a

instance Strengthen (W322Block UV) (W322Block V) where
    strengthen (W322Block a) = do
        a' <- traverse (traverse strengthen) a -- strengthen integers
        case traverse lenPfxFromList a' of -- strengthen inner lists
          Nothing -> Left "TODO list sizing error"
          Just b  -> do
            case lenPfxFromList b of -- strengthen outer list
              Nothing -> Left "TODO list sizing error 2"
              Just  c -> Right $ W322Block c

lenPfxToList :: LenPfx size end a -> [a]
lenPfxToList (LenPfx v) = Vector.toList v

data Seg05Text (v :: Validation) a = Seg05Text
  { seg05TextSpeakerUnkCharID :: Sw v W8
  , seg05TextSpeakerID :: Sw v W32
  , seg05TextText :: a
  , seg05TextVoiceLine :: a
  , seg05TextCounter :: Sw v W32
  } deriving stock (Generic)

deriving stock instance Eq   a => Eq   (Seg05Text UV a)
deriving stock instance Show a => Show (Seg05Text UV a)
deriving stock instance Functor     (Seg05Text UV)
deriving stock instance Foldable    (Seg05Text UV)
deriving stock instance Traversable (Seg05Text UV)

instance BLen a => BLen (Seg05Text V a) where blen = blenGeneric brcNoSum
instance Put  a => Put  (Seg05Text V a) where put  = putGeneric  brcNoSum
instance Get  a => Get  (Seg05Text V a) where get  = getGeneric  brcNoSum

jcSeg05Text :: Aeson.Options
jcSeg05Text = jcProd $ drop $ length ("seg05Text" :: String)

instance ToJSON   a => ToJSON   (Seg05Text UV a) where
    toJSON     = genericToJSON     jcSeg05Text
    toEncoding = genericToEncoding jcSeg05Text
instance FromJSON a => FromJSON (Seg05Text UV a) where
    parseJSON  = genericParseJSON  jcSeg05Text

instance Weaken     (Seg05Text V  a) (Seg05Text UV a) where weaken     = weakenGeneric
instance Strengthen (Seg05Text UV a) (Seg05Text V  a) where strengthen = strengthenGeneric

data Seg (v :: Validation) a
  = Seg00
  | Seg01BG a (Sw v W8) (Sw v W8)
  | Seg02SFX a (Sw v W8)
  | Seg03 (Sw v W8) a (Sw v W8)
  | Seg04 (Sw v W8) (Sw v W8)

  | Seg05 (Seg05Text v a)
  -- ^ Includes player-facing text.

  -- no 0x06

  | Seg07SCP a
  | Seg08

  | Seg09Choice (Sw v W8) (AW32Pairs v a)
  -- ^ Includes player-facing text. Choice selection. The 'W32's appear to be
  --   the same counter as textboxes!
  --
  -- The 'W8' seems to be an file-unique identifier for the choice selection.
  -- SCP files with multiple choices have 0, 1, 2 etc. in ascending order.

  | Seg0A (W322Block v)

  | Seg0B (Sw v W8) (Sw v W8)
  -- ^ Appears to indicate where a given choice jumps to.
  --
  -- First 'W8' appears to specify which choice selection it relates to.
  -- Second 'W8' appears to be the choice index in that choice selection
  -- (usually 0, 1). They may be highly separated, in cases where a choice
  -- changes lots of dialog.

  | Seg0CFlag (Sw v W8) (Sw v W8)
  | Seg0D (Sw v W8)
  | Seg0E (Sw v W8)
  | Seg0F
  | Seg10 (Sw v W8) (Sw v W8) (Sw v W8)
  | Seg11EventCG a
  | Seg12

  | Seg13 (Sw v W8) a (Sw v W8) (Sw v W32)
  -- ^ Possibly player-facing text. Registers words for the feast
  --   Danganronpa-style minigame, but using indices which correspond to
  --   textures with the text on. The usage of the text here is unknown.

  | Seg14 (Sw v W8)
  | Seg15
  | Seg16Wadai
  | Seg17 (Sw v W8) (Sw v W8)
  | Seg18 (Sw v W8) (Sw v W8)
  | Seg19 (Sw v W8) (Sw v W8)
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  | Seg1D
  | Seg1E (Sw v W8)
  | Seg1FDelay
  | Seg20 (Sw v W8)
  | Seg21

  | Seg22 a (AW32Pairs v a)
  -- ^ Includes player-facing text. Choice, plus an extra string. Seem to be
  --   used in the conversation events.

  | Seg23SFX

  | Seg24 a
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg25
  | Seg26

  | Seg27 a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg28 a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg29 a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg2A (Sw v W8) (Sw v W8)

  | Seg2B (Sw v W8) (Sw v W8) (Sw v W8) a (W322Block v)

  | Seg2CMap

  | Seg2D a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg2E (Sw v W8) (Sw v W8) (Sw v W32) (Sw v W32)
  | Seg2F (Sw v W8)
  | Seg30 (Sw v W8)
  | Seg31
  | Seg32 (Sw v W32) (Sw v W8)
  | Seg33 (Sw v W8) (Sw v W8)
  | Seg34 (Sw v W32)

  | Seg35 a
  -- ^ Likely player-facing. Kinda sounds like it's a type of Banri choice.

  | Seg36 (Sw v W8) (Sw v W32)
  | Seg37 (Sw v W8) (Sw v W32)
  | Seg38 (Sw v W8) (Sw v W32)
  | Seg39
  | Seg3A (Sw v W8) (Sw v W8)
  | Seg3B (Sw v W8)
  | Seg3C (Sw v W8) (Sw v W8)
  | Seg3D
  | Seg3E (Sw v W8) (Sw v W8)
  | Seg3F (Sw v W8) (Sw v W32) (Sw v W32)
  | Seg40 (Sw v W8)
  | Seg41 (Sw v W8) (Sw v W32) (Sw v W32)
  | Seg42 (Sw v W8)

  -- these don't appear very SFXy in code, but did in data
  | Seg43SFX a
  | Seg44SFX a
  | Seg45SFX a (Sw v W8)

  | Seg46 (Sw v W8) (Sw v W8)
  | Seg47 (Sw v W8) (Sw v W8)
  | Seg48
  | Seg49
  | Seg4A
  | Seg4B

  | Seg4C a
  -- Unknown. Appears unused.

  | Seg4D
  | Seg4E

  | Seg4F a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg50 a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg51 a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg52 a
  -- ^ Text not user-facing. Refers to a @sound/voice@ file - all the uses I've
  --   seen are girls telling you 飲んで飲んで！

  | Seg53 a (Sw v W8) (Sw v W8)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg54 (Sw v W8) (Sw v W8)
  | Seg55 (Sw v W8) (Sw v W8)
  | Seg56 (Sw v W8) (Sw v W8)
  | Seg57 (Sw v W8) (Sw v W8)
  | Seg58 (Sw v W8) (Sw v W8)
  | Seg59 (Sw v W8)
  | Seg5A (Sw v W32) (Sw v W8)
  | Seg5B (Sw v W32)
  | Seg5C (Sw v W8)
  | Seg5D (Sw v W32)
  | Seg5E (Sw v W32)
  | Seg5F (Sw v W32)
  | Seg60 (Sw v W32)
  | Seg61 (Sw v W8) (Sw v W8)
  | Seg62 (Sw v W8) -- 0x01 <= w8 <= 0x11, no default
  | Seg63
  | Seg64
  | Seg65Trophy
  | Seg66
  | Seg67
  | Seg68
  | Seg69
  | Seg6A (Sw v W8)
  | Seg6B (Sw v W8)
  | Seg6CWipe (Sw v W8) (Sw v W32) (Sw v W32) (Sw v W32)
  | Seg6DWipe (Sw v W8) (Sw v W32) (Sw v W32) (Sw v W32)
  | Seg6E
  | Seg6F
  | Seg70 (Sw v W8) (Sw v W8)
  | Seg71 (Sw v W8) (Sw v W8)
  | Seg72
  | Seg73Kyoro (Sw v W8) (Sw v W32) -- 0x01 <= w8 <= 0x06, with default
  | Seg74
  | Seg75 (Sw v W8)
  | Seg76
  | Seg77SCP (Sw v W8)
    deriving stock (Generic)

deriving stock instance Show a => Show (Seg UV a)
deriving stock instance Eq   a => Eq   (Seg UV a)

brcSeg :: BR.Cfg W8
brcSeg = BR.Cfg { BR.cSumTag = BR.cSumTagHex extractTagByte }
  where extractTagByte = take 2 . drop (length ("Seg" :: String))

instance BLen a => BLen (Seg V a) where blen = blenGeneric brcSeg
instance Put  a => Put  (Seg V a) where put  = putGeneric  brcSeg
instance Get  a => Get  (Seg V a) where get  = getGeneric  brcSeg

jcSeg :: Aeson.Options
jcSeg = jcSum id (take 2 . drop (length ("Seg" :: String))) "command_byte" "arguments"

instance ToJSON   a => ToJSON   (Seg UV a) where
    toJSON     = genericToJSON     jcSeg
    toEncoding = genericToEncoding jcSeg
instance FromJSON a => FromJSON (Seg UV a) where
    parseJSON  = genericParseJSON  jcSeg

type SCP v a = [Seg v a]
type SCPBin  = SCP V (AsByteString 'C)
type SCPText = SCP UV (AsText 'UTF8)

scpFmap :: (a -> b) -> SCP UV a -> SCP UV b
scpFmap = fmap . fmap

scpTraverse :: Applicative f => (a -> f b) -> SCP UV a -> f (SCP UV b)
scpTraverse = traverse . traverse

prettyYamlCfg :: Yaml.Pretty.Config
prettyYamlCfg =
      Yaml.Pretty.setConfCompare f
    $ Yaml.Pretty.setConfDropNull True
    $ Yaml.Pretty.defConfig
  where
    f "command_byte" _ = LT
    f _ "command_byte" = GT
    f k1 k2 = compare k1 k2

deriving stock instance Functor     (Seg UV)
deriving stock instance Foldable    (Seg UV)
deriving stock instance Traversable (Seg UV)

instance Weaken     (Seg V  a) (Seg UV a) where weaken     = weakenGeneric
instance Strengthen (Seg UV a) (Seg V  a) where strengthen = strengthenGeneric
