{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP where

import GHC.Generics ( Generic )
import Data.Data ( Typeable )
import Data.Aeson qualified as Aeson
import Data.Aeson ( ToJSON(..), genericToJSON, genericToEncoding, FromJSON(..), genericParseJSON )
import Binrep
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int ( I(..), ISize(..), ISign(..) )
import Binrep.Type.ByteString ( Rep(..) )
import Data.Yaml.Pretty qualified as Yaml.Pretty

import Refined

type Endian = 'LE
type W8  = I 'U 'I1 Endian
type W32 = I 'U 'I4 Endian
type PfxLenW8 = 'Pascal 'I1 Endian
type AW32Pairs v a = WithRefine v PfxLenW8 [(a, W32)]

type V = 'Validated
type UV = 'Unvalidated

brcNoSum :: Binrep.Cfg W8
brcNoSum = Binrep.Cfg { Binrep.cSumTag = undefined }

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

type W322Block (v :: Validation) =
    WithRefine v PfxLenW8 [WithRefine v PfxLenW8 [W32]]

data Seg05Text a = Seg05Text
  { seg05TextSpeakerUnkCharID :: W8
  , seg05TextSpeakerID :: W32
  , seg05TextText :: a
  , seg05TextVoiceLine :: a
  , seg05TextCounter :: W32
  } deriving stock (Generic, Eq, Show, Foldable)

deriving stock instance Functor     Seg05Text
deriving stock instance Traversable Seg05Text

instance BLen a => BLen (Seg05Text a) where blen = blenGeneric brcNoSum
instance Put  a => Put  (Seg05Text a) where put  = putGeneric  brcNoSum
instance Get  a => Get  (Seg05Text a) where get  = getGeneric  brcNoSum

jcSeg05Text :: Aeson.Options
jcSeg05Text = jcProd $ drop $ length ("seg05Text" :: String)

instance ToJSON   a => ToJSON   (Seg05Text a) where
    toJSON     = genericToJSON     jcSeg05Text
    toEncoding = genericToEncoding jcSeg05Text
instance FromJSON a => FromJSON (Seg05Text a) where
    parseJSON  = genericParseJSON  jcSeg05Text

data Seg (v :: Validation) a
  = Seg00
  | Seg01BG a W8 W8
  | Seg02SFX a W8
  | Seg03 W8 a W8
  | Seg04 W8 W8

  | Seg05 (Seg05Text a)
  -- ^ Includes player-facing text.

  -- no 0x06

  | Seg07SCP a
  | Seg08

  | Seg09Choice W8 (AW32Pairs v a)
  -- ^ Includes player-facing text. Choice selection. The 'W32's appear to be
  --   the same counter as textboxes!
  --
  -- The 'W8' seems to be an file-unique identifier for the choice selection.
  -- SCP files with multiple choices have 0, 1, 2 etc. in ascending order.

  | Seg0A (W322Block v)

  | Seg0B W8 W8
  -- ^ Appears to indicate where a given choice jumps to.
  --
  -- First 'W8' appears to specify which choice selection it relates to.
  -- Second 'W8' appears to be the choice index in that choice selection
  -- (usually 0, 1). They may be highly separated, in cases where a choice
  -- changes lots of dialog.

  | Seg0CFlag W8 W8
  | Seg0D W8
  | Seg0E W8
  | Seg0F
  | Seg10 W8 W8 W8
  | Seg11EventCG a
  | Seg12

  | Seg13 W8 a W8 W32
  -- ^ Possibly player-facing text. Registers words for the feast
  --   Danganronpa-style minigame, but using indices which correspond to
  --   textures with the text on. The usage of the text here is unknown.

  | Seg14 W8
  | Seg15
  | Seg16Wadai
  | Seg17 W8 W8
  | Seg18 W8 W8
  | Seg19 W8 W8
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  | Seg1D
  | Seg1E W8
  | Seg1FDelay
  | Seg20 W8
  | Seg21

  | Seg22 a (AW32Pairs v a)
  -- ^ Includes player-facing text. Choice, plus an extra string. Seem to be
  --   used in the conversation events.

  | Seg23SFX

  | Seg24 a
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg25
  | Seg26

  | Seg27 a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg28 a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg29 a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg2A W8 W8

  | Seg2B W8 W8 W8 a (W322Block v)

  | Seg2CMap

  | Seg2D a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg2E W8 W8 W32 W32
  | Seg2F W8
  | Seg30 W8
  | Seg31
  | Seg32 W32 W8
  | Seg33 W8 W8
  | Seg34 W32

  | Seg35 a
  -- ^ Likely player-facing. Kinda sounds like it's a type of Banri choice.

  | Seg36 W8 W32
  | Seg37 W8 W32
  | Seg38 W8 W32
  | Seg39
  | Seg3A W8 W8
  | Seg3B W8
  | Seg3C W8 W8
  | Seg3D
  | Seg3E W8 W8
  | Seg3F W8 W32 W32
  | Seg40 W8
  | Seg41 W8 W32 W32
  | Seg42 W8

  -- these don't appear very SFXy in code, but did in data
  | Seg43SFX a
  | Seg44SFX a
  | Seg45SFX a W8

  | Seg46 W8 W8
  | Seg47 W8 W8
  | Seg48
  | Seg49
  | Seg4A
  | Seg4B

  | Seg4C a
  -- Unknown. Appears unused.

  | Seg4D
  | Seg4E

  | Seg4F a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg50 a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg51 a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg52 a
  -- ^ Text not user-facing. Refers to a @sound/voice@ file - all the uses I've
  --   seen are girls telling you 飲んで飲んで！

  | Seg53 a W8 W8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | Seg54 W8 W8
  | Seg55 W8 W8
  | Seg56 W8 W8
  | Seg57 W8 W8
  | Seg58 W8 W8
  | Seg59 W8
  | Seg5A W32 W8
  | Seg5B W32
  | Seg5C W8
  | Seg5D W32
  | Seg5E W32
  | Seg5F W32
  | Seg60 W32
  | Seg61 W8 W8
  | Seg62 W8 -- 0x01 <= w8 <= 0x11, no default
  | Seg63
  | Seg64
  | Seg65Trophy
  | Seg66
  | Seg67
  | Seg68
  | Seg69
  | Seg6A W8
  | Seg6B W8
  | Seg6CWipe W8 W32 W32 W32
  | Seg6DWipe W8 W32 W32 W32
  | Seg6E
  | Seg6F
  | Seg70 W8 W8
  | Seg71 W8 W8
  | Seg72
  | Seg73Kyoro W8 W32 -- 0x01 <= w8 <= 0x06, with default
  | Seg74
  | Seg75 W8
  | Seg76
  | Seg77SCP W8
    deriving stock (Generic, Show, Eq, Foldable)

deriving stock instance Functor     (Seg UV)
deriving stock instance Traversable (Seg UV)

brcSeg :: Binrep.Cfg W8
brcSeg = Binrep.Cfg { Binrep.cSumTag = Binrep.cSumTagHex extractTagByte }
  where extractTagByte = take 2 . drop (length ("Seg" :: String))

instance BLen a => BLen (Seg V a) where blen = blenGeneric brcSeg
instance Put  a => Put  (Seg V a) where put  = putGeneric  brcSeg
instance Get  a => Get  (Seg V a) where get  = getGeneric  brcSeg

jcSeg :: Aeson.Options
jcSeg = jcSum id (take 2 . drop (length ("Seg" :: String))) "command_byte" "arguments"

instance ToJSON   a => ToJSON   (Seg v a) where
    toJSON     = genericToJSON     jcSeg
    toEncoding = genericToEncoding jcSeg
instance FromJSON a => FromJSON (Seg UV a) where
    parseJSON  = genericParseJSON  jcSeg
instance (FromJSON a, Typeable a) => FromJSON (Seg V a) where
    parseJSON  = genericParseJSON  jcSeg

type SCP v a = [Seg v a]

prettyYamlCfg :: Yaml.Pretty.Config
prettyYamlCfg =
      Yaml.Pretty.setConfCompare f
    $ Yaml.Pretty.setConfDropNull True
    $ Yaml.Pretty.defConfig
  where
    f "command_byte" _ = LT
    f _ "command_byte" = GT
    f k1 k2 = compare k1 k2

traverseSCP :: Applicative t => (a -> t b) -> SCP UV a -> t (SCP UV b)
traverseSCP f = traverse (traverse f)

refineSCP :: Typeable a => SCP UV a -> Either RefineException (SCP V a)
refineSCP = traverse refineSeg

refineAW32Pairs :: Typeable a => AW32Pairs UV a -> Either RefineException (AW32Pairs V a)
refineAW32Pairs = refineWith

unrefineAW32Pairs :: AW32Pairs V a -> AW32Pairs UV a
unrefineAW32Pairs = unrefineWith

refineW322Block :: W322Block UV -> Either RefineException (W322Block V)
refineW322Block d = do
    d' <- traverse refineWith $ withoutRefine d
    refine d'

unrefineW322Block :: W322Block V -> W322Block UV
unrefineW322Block d = do
    let d' = fmap unrefineWith $ withoutRefine d
    withRefine d'

refineSeg :: Typeable a => Seg UV a -> Either RefineException (Seg V a)
refineSeg = \case
  Seg00 -> r Seg00
  Seg01BG a1 w1 w2 -> r $ Seg01BG a1 w1 w2
  Seg02SFX a1 w1 -> r $ Seg02SFX a1 w1
  Seg09Choice w1 d1 -> do
    d1' <- refineAW32Pairs d1
    r $ Seg09Choice w1 d1'
  Seg03 w1 a1 w2 -> r $ Seg03 w1 a1 w2
  Seg04 w1 w2 -> r $ Seg04 w1 w2
  Seg05 d1 -> r $ Seg05 d1
  -- no 0x06
  Seg07SCP a1 -> r $ Seg07SCP a1
  Seg08 -> r Seg08
  Seg0A d1 -> do
      d1' <- refineW322Block d1
      r $ Seg0A d1'
  Seg0B w1 w2 -> r $ Seg0B w1 w2
  Seg0CFlag w1 w2 -> r $ Seg0CFlag w1 w2
  Seg0D w1 -> r $ Seg0D w1
  Seg0E w1 -> r $ Seg0E w1
  Seg0F -> r Seg0F
  Seg10 w1 w2 w3 -> r $ Seg10 w1 w2 w3
  Seg11EventCG a1 -> r $ Seg11EventCG a1
  Seg12 -> r Seg12
  Seg13 w1 a1 w2 w3 -> r $ Seg13 w1 a1 w2 w3
  Seg14 w1 -> r $ Seg14 w1
  Seg15 -> r Seg15
  Seg16Wadai -> r Seg16Wadai
  Seg17 w1 w2 -> r $ Seg17 w1 w2
  Seg18 w1 w2 -> r $ Seg18 w1 w2
  Seg19 w1 w2 -> r $ Seg19 w1 w2
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  Seg1D -> r Seg1D
  Seg1E w1 -> r $ Seg1E w1
  Seg1FDelay -> r Seg1FDelay
  Seg20 w1 -> r $ Seg20 w1
  Seg21 -> r Seg21
  Seg22 a1 d1 -> do
    d1' <- refineAW32Pairs d1
    r $ Seg22 a1 d1'
  Seg23SFX -> r Seg23SFX
  Seg24 a1 -> r $ Seg24 a1
  Seg25 -> r Seg25
  Seg26 -> r Seg26
  Seg27 a1 w1 w2 -> r $ Seg27 a1 w1 w2
  Seg28 a1 w1 w2 -> r $ Seg28 a1 w1 w2
  Seg29 a1 w1 w2 -> r $ Seg29 a1 w1 w2
  Seg2A w1 w2 -> r $ Seg2A w1 w2
  Seg2B w1 w2 w3 a1 d1 -> do
    d1' <- refineW322Block d1
    r $ Seg2B w1 w2 w3 a1 d1'
  Seg2CMap -> r Seg2CMap
  Seg2D a1 w1 w2 -> r $ Seg2D a1 w1 w2
  Seg2E w1 w2 w3 w4 -> r $ Seg2E w1 w2 w3 w4
  Seg2F w1 -> r $ Seg2F w1
  Seg30 w1 -> r $ Seg30 w1
  Seg31 -> r Seg31
  Seg32 w1 w2 -> r $ Seg32 w1 w2
  Seg33 w1 w2 -> r $ Seg33 w1 w2
  Seg34 w1 -> r $ Seg34 w1
  Seg35 a1 -> r $ Seg35 a1
  Seg36 w1 w2 -> r $ Seg36 w1 w2
  Seg37 w1 w2 -> r $ Seg37 w1 w2
  Seg38 w1 w2 -> r $ Seg38 w1 w2
  Seg39 -> r $ Seg39
  Seg3A w1 w2 -> r $ Seg3A w1 w2
  Seg3B w1 -> r $ Seg3B w1
  Seg3C w1 w2 -> r $ Seg3C w1 w2
  Seg3D -> r $ Seg3D
  Seg3E w1 w2 -> r $ Seg3E w1 w2
  Seg3F w1 w2 w3 -> r $ Seg3F w1 w2 w3
  Seg40 w1 -> r $ Seg40 w1
  Seg41 w1 w2 w3 -> r $ Seg41 w1 w2 w3
  Seg42 w1 -> r $ Seg42 w1
  Seg43SFX a1 -> r $ Seg43SFX a1
  Seg44SFX a1 -> r $ Seg44SFX a1
  Seg45SFX a1 w1 -> r $ Seg45SFX a1 w1
  Seg46 w1 w2 -> r $ Seg46 w1 w2
  Seg47 w1 w2 -> r $ Seg47 w1 w2
  Seg48 -> r $ Seg48
  Seg49 -> r $ Seg49
  Seg4A -> r $ Seg4A
  Seg4B -> r $ Seg4B
  Seg4C a1 -> r $ Seg4C a1
  Seg4D -> r $ Seg4D
  Seg4E -> r $ Seg4E
  Seg4F a1 w1 w2 -> r $ Seg4F a1 w1 w2
  Seg50 a1 w1 w2 -> r $ Seg50 a1 w1 w2
  Seg51 a1 w1 w2 -> r $ Seg51 a1 w1 w2
  Seg52 a1 -> r $ Seg52 a1
  Seg53 a1 w1 w2 -> r $ Seg53 a1 w1 w2
  Seg54 w1 w2 -> r $ Seg54 w1 w2
  Seg55 w1 w2 -> r $ Seg55 w1 w2
  Seg56 w1 w2 -> r $ Seg56 w1 w2
  Seg57 w1 w2 -> r $ Seg57 w1 w2
  Seg58 w1 w2 -> r $ Seg58 w1 w2
  Seg59 w1 -> r $ Seg59 w1
  Seg5A w1 w2 -> r $ Seg5A w1 w2
  Seg5B w1 -> r $ Seg5B w1
  Seg5C w1 -> r $ Seg5C w1
  Seg5D w1 -> r $ Seg5D w1
  Seg5E w1 -> r $ Seg5E w1
  Seg5F w1 -> r $ Seg5F w1
  Seg60 w1 -> r $ Seg60 w1
  Seg61 w1 w2 -> r $ Seg61 w1 w2
  Seg62 w1 -> r $ Seg62 w1
  Seg63 -> r $ Seg63
  Seg64 -> r $ Seg64
  Seg65Trophy -> r $ Seg65Trophy
  Seg66 -> r $ Seg66
  Seg67 -> r $ Seg67
  Seg68 -> r $ Seg68
  Seg69 -> r $ Seg69
  Seg6A w1 -> r $ Seg6A w1
  Seg6B w1 -> r $ Seg6B w1
  Seg6CWipe w1 w2 w3 w4 -> r $ Seg6CWipe w1 w2 w3 w4
  Seg6DWipe w1 w2 w3 w4 -> r $ Seg6DWipe w1 w2 w3 w4
  Seg6E -> r $ Seg6E
  Seg6F -> r $ Seg6F
  Seg70 w1 w2 -> r $ Seg70 w1 w2
  Seg71 w1 w2 -> r $ Seg71 w1 w2
  Seg72 -> r $ Seg72
  Seg73Kyoro w1 w2 -> r $ Seg73Kyoro w1 w2
  Seg74 -> r $ Seg74
  Seg75 w1 -> r $ Seg75 w1
  Seg76 -> r $ Seg76
  Seg77SCP w1 -> r $ Seg77SCP w1
  where r = Right

unrefineSCP :: SCP V a -> SCP UV a
unrefineSCP = fmap unrefineSeg

unrefineSeg :: Seg V a -> Seg UV a
unrefineSeg = \case
  Seg00 -> r Seg00
  Seg01BG a1 w1 w2 -> r $ Seg01BG a1 w1 w2
  Seg02SFX a1 w1 -> r $ Seg02SFX a1 w1
  Seg09Choice w1 d1 -> do
    let d1' = unrefineAW32Pairs d1
    r $ Seg09Choice w1 d1'
  Seg03 w1 a1 w2 -> r $ Seg03 w1 a1 w2
  Seg04 w1 w2 -> r $ Seg04 w1 w2
  Seg05 d1 -> r $ Seg05 d1
  -- no 0x06
  Seg07SCP a1 -> r $ Seg07SCP a1
  Seg08 -> r Seg08
  Seg0A d1 -> do
    let d1' = unrefineW322Block d1
    r $ Seg0A d1'
  Seg0B w1 w2 -> r $ Seg0B w1 w2
  Seg0CFlag w1 w2 -> r $ Seg0CFlag w1 w2
  Seg0D w1 -> r $ Seg0D w1
  Seg0E w1 -> r $ Seg0E w1
  Seg0F -> r Seg0F
  Seg10 w1 w2 w3 -> r $ Seg10 w1 w2 w3
  Seg11EventCG a1 -> r $ Seg11EventCG a1
  Seg12 -> r Seg12
  Seg13 w1 a1 w2 w3 -> r $ Seg13 w1 a1 w2 w3
  Seg14 w1 -> r $ Seg14 w1
  Seg15 -> r Seg15
  Seg16Wadai -> r Seg16Wadai
  Seg17 w1 w2 -> r $ Seg17 w1 w2
  Seg18 w1 w2 -> r $ Seg18 w1 w2
  Seg19 w1 w2 -> r $ Seg19 w1 w2
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  Seg1D -> r Seg1D
  Seg1E w1 -> r $ Seg1E w1
  Seg1FDelay -> r Seg1FDelay
  Seg20 w1 -> r $ Seg20 w1
  Seg21 -> r Seg21
  Seg22 a1 d1 -> do
    let d1' = unrefineAW32Pairs d1
    r $ Seg22 a1 d1'
  Seg23SFX -> r Seg23SFX
  Seg24 a1 -> r $ Seg24 a1
  Seg25 -> r Seg25
  Seg26 -> r Seg26
  Seg27 a1 w1 w2 -> r $ Seg27 a1 w1 w2
  Seg28 a1 w1 w2 -> r $ Seg28 a1 w1 w2
  Seg29 a1 w1 w2 -> r $ Seg29 a1 w1 w2
  Seg2A w1 w2 -> r $ Seg2A w1 w2
  Seg2B w1 w2 w3 a1 d1 -> do
    let d1' = unrefineW322Block d1
    r $ Seg2B w1 w2 w3 a1 d1'
  Seg2CMap -> r Seg2CMap
  Seg2D a1 w1 w2 -> r $ Seg2D a1 w1 w2
  Seg2E w1 w2 w3 w4 -> r $ Seg2E w1 w2 w3 w4
  Seg2F w1 -> r $ Seg2F w1
  Seg30 w1 -> r $ Seg30 w1
  Seg31 -> r Seg31
  Seg32 w1 w2 -> r $ Seg32 w1 w2
  Seg33 w1 w2 -> r $ Seg33 w1 w2
  Seg34 w1 -> r $ Seg34 w1
  Seg35 a1 -> r $ Seg35 a1
  Seg36 w1 w2 -> r $ Seg36 w1 w2
  Seg37 w1 w2 -> r $ Seg37 w1 w2
  Seg38 w1 w2 -> r $ Seg38 w1 w2
  Seg39 -> r $ Seg39
  Seg3A w1 w2 -> r $ Seg3A w1 w2
  Seg3B w1 -> r $ Seg3B w1
  Seg3C w1 w2 -> r $ Seg3C w1 w2
  Seg3D -> r $ Seg3D
  Seg3E w1 w2 -> r $ Seg3E w1 w2
  Seg3F w1 w2 w3 -> r $ Seg3F w1 w2 w3
  Seg40 w1 -> r $ Seg40 w1
  Seg41 w1 w2 w3 -> r $ Seg41 w1 w2 w3
  Seg42 w1 -> r $ Seg42 w1
  Seg43SFX a1 -> r $ Seg43SFX a1
  Seg44SFX a1 -> r $ Seg44SFX a1
  Seg45SFX a1 w1 -> r $ Seg45SFX a1 w1
  Seg46 w1 w2 -> r $ Seg46 w1 w2
  Seg47 w1 w2 -> r $ Seg47 w1 w2
  Seg48 -> r $ Seg48
  Seg49 -> r $ Seg49
  Seg4A -> r $ Seg4A
  Seg4B -> r $ Seg4B
  Seg4C a1 -> r $ Seg4C a1
  Seg4D -> r $ Seg4D
  Seg4E -> r $ Seg4E
  Seg4F a1 w1 w2 -> r $ Seg4F a1 w1 w2
  Seg50 a1 w1 w2 -> r $ Seg50 a1 w1 w2
  Seg51 a1 w1 w2 -> r $ Seg51 a1 w1 w2
  Seg52 a1 -> r $ Seg52 a1
  Seg53 a1 w1 w2 -> r $ Seg53 a1 w1 w2
  Seg54 w1 w2 -> r $ Seg54 w1 w2
  Seg55 w1 w2 -> r $ Seg55 w1 w2
  Seg56 w1 w2 -> r $ Seg56 w1 w2
  Seg57 w1 w2 -> r $ Seg57 w1 w2
  Seg58 w1 w2 -> r $ Seg58 w1 w2
  Seg59 w1 -> r $ Seg59 w1
  Seg5A w1 w2 -> r $ Seg5A w1 w2
  Seg5B w1 -> r $ Seg5B w1
  Seg5C w1 -> r $ Seg5C w1
  Seg5D w1 -> r $ Seg5D w1
  Seg5E w1 -> r $ Seg5E w1
  Seg5F w1 -> r $ Seg5F w1
  Seg60 w1 -> r $ Seg60 w1
  Seg61 w1 w2 -> r $ Seg61 w1 w2
  Seg62 w1 -> r $ Seg62 w1
  Seg63 -> r $ Seg63
  Seg64 -> r $ Seg64
  Seg65Trophy -> r $ Seg65Trophy
  Seg66 -> r $ Seg66
  Seg67 -> r $ Seg67
  Seg68 -> r $ Seg68
  Seg69 -> r $ Seg69
  Seg6A w1 -> r $ Seg6A w1
  Seg6B w1 -> r $ Seg6B w1
  Seg6CWipe w1 w2 w3 w4 -> r $ Seg6CWipe w1 w2 w3 w4
  Seg6DWipe w1 w2 w3 w4 -> r $ Seg6DWipe w1 w2 w3 w4
  Seg6E -> r $ Seg6E
  Seg6F -> r $ Seg6F
  Seg70 w1 w2 -> r $ Seg70 w1 w2
  Seg71 w1 w2 -> r $ Seg71 w1 w2
  Seg72 -> r $ Seg72
  Seg73Kyoro w1 w2 -> r $ Seg73Kyoro w1 w2
  Seg74 -> r $ Seg74
  Seg75 w1 -> r $ Seg75 w1
  Seg76 -> r $ Seg76
  Seg77SCP w1 -> r $ Seg77SCP w1
  where r = id
