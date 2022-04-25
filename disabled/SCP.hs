{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP
  ( SCP
  , SCPSeg(..)
  , SCPSeg05Textbox(..)
  , SCPSeg0ADyn(..)

  , encodeYamlPretty
  ) where

import GHC.Generics ( Generic )
import Data.Typeable ( Typeable )
import GTVM.Common.Json
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Text ( Text )

import Data.Aeson
import Data.Yaml.Pretty qualified as YamlPretty

import Binrep.Types.Int
import Binrep.Types.Common ( Endianness(..) )
import Binrep.Types.ByteString

import Refined
import Refined.WithRefine

-- | A SCP file.
--
-- SCPs are ordered lists of individual segments (commands, macros, whatever).
-- Segments are identified by a single prefix byte. Once the prefix byte is
-- recognized, the segment is parsed accordingly, then the next prefix byte is
-- read, and so on until it finds EOF instead of a prefix byte.
type SCP e ps bs = [SCPSeg e ps bs]

type W8  e = I 'U 'I1 e
type W32 e = I 'U 'I4 e
type WR = WithRefine

-- | A standalone segment of an SCP file.
--
-- Most segments are straightforward, with minimal allocation required (usually
-- just strings). Some have dynamic parts, where extra parsing is done depending
-- on a value - all or mostly some form of lists.
--
-- Where useful (due to complexity, or because it's an especially interesting
-- segment), inner parts are spun out into their own data type.
--
-- Generalized over the bytestring representation (for easier coercing between
-- 'ByteString' and 'Text').
data SCPSeg (e :: Endianness) (ps :: PredicateStatus) bs
  = SCPSeg00
  | SCPSeg01BG bs (W8 e) (W8 e)
  | SCPSeg02SFX bs (W8 e)
  | SCPSeg03 (W8 e) bs (W8 e)
  | SCPSeg04 (W8 e) (W8 e)

  | SCPSeg05Textbox (SCPSeg05Textbox e bs)
  -- ^ Includes player-facing text.

  -- no 0x06

  | SCPSeg07SCP bs
  | SCPSeg08

  | SCPSeg09Choice (W8 e) (WR ps ('Pascal 'I1 e) [(bs, W32 e)])
  -- ^ Includes player-facing text. Choice selection. The 'W32's appear to be
  --   the same counter as textboxes!
  --
  -- The '(W8 e)' seems to be an file-unique identifier for the choice selection.
  -- SCP files with multiple choices have 0, 1, 2 etc. in ascending order.

  | SCPSeg0A (SCPSeg0ADyn e ps bs)

  | SCPSeg0B (W8 e) (W8 e)
  -- ^ Appears to indicate where a given choice jumps to.
  --
  -- First 'W8' appears to specify which choice selection it relates to.
  -- Second 'W8' appears to be the choice index in that choice selection
  -- (usually 0, 1). They may be highly separated, in cases where a choice
  -- changes lots of dialog.

  | SCPSeg0CFlag (W8 e) (W8 e)
  | SCPSeg0D (W8 e)
  | SCPSeg0E (W8 e)
  | SCPSeg0F
  | SCPSeg10 (W8 e) (W8 e) (W8 e)
  | SCPSeg11EventCG bs
  | SCPSeg12

  | SCPSeg13 (W8 e) bs (W8 e) (W32 e)
  -- ^ Possibly player-facing text. Registers words for the feast
  --   Danganronpa-style minigame, but using indices which correspond to
  --   textures with the text on. The usage of the text here is unknown.

  | SCPSeg14 (W8 e)
  | SCPSeg15
  | SCPSeg16Wadai
  | SCPSeg17 (W8 e) (W8 e)
  | SCPSeg18 (W8 e) (W8 e)
  | SCPSeg19 (W8 e) (W8 e)
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  | SCPSeg1D
  | SCPSeg1E (W8 e)
  | SCPSeg1FDelay
  | SCPSeg20 (W8 e)
  | SCPSeg21

  | SCPSeg22 bs (WR ps ('Pascal 'I1 e) [(bs, W32 e)])
  -- ^ Includes player-facing text. Choice, plus an extra string. Seem to be
  --   used in the conversation events.

  | SCPSeg23SFX

  | SCPSeg24 bs
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg25
  | SCPSeg26

  | SCPSeg27 bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg28 bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg29 bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg2A (W8 e) (W8 e)

  | SCPSeg2B (W8 e) (W8 e) (W8 e) bs (SCPSeg0ADyn e ps bs)

  | SCPSeg2CMap

  | SCPSeg2D bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg2E (W8 e) (W8 e) (W32 e) (W32 e)
  | SCPSeg2F (W8 e)
  | SCPSeg30 (W8 e)
  | SCPSeg31
  | SCPSeg32 (W32 e) (W8 e)
  | SCPSeg33 (W8 e) (W8 e)
  | SCPSeg34 (W32 e)

  | SCPSeg35 bs
  -- ^ Likely player-facing. Kinda sounds like it's a type of Banri choice.

  | SCPSeg36 (W8 e) (W32 e)
  | SCPSeg37 (W8 e) (W32 e)
  | SCPSeg38 (W8 e) (W32 e)
  | SCPSeg39
  | SCPSeg3A (W8 e) (W8 e)
  | SCPSeg3B (W8 e)
  | SCPSeg3C (W8 e) (W8 e)
  | SCPSeg3D
  | SCPSeg3E (W8 e) (W8 e)
  | SCPSeg3F (W8 e) (W32 e) (W32 e)
  | SCPSeg40 (W8 e)
  | SCPSeg41 (W8 e) (W32 e) (W32 e)
  | SCPSeg42 (W8 e)

  -- these don't appear very SFXy in code, but did in data
  | SCPSeg43SFX bs
  | SCPSeg44SFX bs
  | SCPSeg45SFX bs (W8 e)

  | SCPSeg46 (W8 e) (W8 e)
  | SCPSeg47 (W8 e) (W8 e)
  | SCPSeg48
  | SCPSeg49
  | SCPSeg4A
  | SCPSeg4B

  | SCPSeg4C bs
  -- Unknown. Appears unused.

  | SCPSeg4D
  | SCPSeg4E

  | SCPSeg4F bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg50 bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg51 bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg52 bs
  -- ^ Text not user-facing. Refers to a @sound/voice@ file - all the uses I've
  --   seen are girls telling you 飲んで飲んで！

  | SCPSeg53 bs (W8 e) (W8 e)
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg54 (W8 e) (W8 e)
  | SCPSeg55 (W8 e) (W8 e)
  | SCPSeg56 (W8 e) (W8 e)
  | SCPSeg57 (W8 e) (W8 e)
  | SCPSeg58 (W8 e) (W8 e)
  | SCPSeg59 (W8 e)
  | SCPSeg5A (W32 e) (W8 e)
  | SCPSeg5B (W32 e)
  | SCPSeg5C (W8 e)
  | SCPSeg5D (W32 e)
  | SCPSeg5E (W32 e)
  | SCPSeg5F (W32 e)
  | SCPSeg60 (W32 e)
  | SCPSeg61 (W8 e) (W8 e)
  | SCPSeg62 (W8 e) -- 0x01 <= w8 <= 0x11, no default
  | SCPSeg63
  | SCPSeg64
  | SCPSeg65Trophy
  | SCPSeg66
  | SCPSeg67
  | SCPSeg68
  | SCPSeg69
  | SCPSeg6A (W8 e)
  | SCPSeg6B (W8 e)
  | SCPSeg6CWipe (W8 e) (W32 e) (W32 e) (W32 e)
  | SCPSeg6DWipe (W8 e) (W32 e) (W32 e) (W32 e)
  | SCPSeg6E
  | SCPSeg6F
  | SCPSeg70 (W8 e) (W8 e)
  | SCPSeg71 (W8 e) (W8 e)
  | SCPSeg72
  | SCPSeg73Kyoro (W8 e) (W32 e) -- 0x01 <= w8 <= 0x06, with default
  | SCPSeg74
  | SCPSeg75 (W8 e)
  | SCPSeg76
  | SCPSeg77SCP (W8 e)
    deriving stock (Generic, Typeable, Show, Foldable, Eq)

deriving stock instance Functor     (SCPSeg e 'Unenforced)
deriving stock instance Traversable (SCPSeg e 'Unenforced)

-- | SCP segment JSON en/decoding config.
--
-- For selecting the constructor, only the 2 hex digits are used.
jcSCPSeg :: Options
jcSCPSeg = defaultOptions
  { constructorTagModifier = take 2 . drop 6
  , sumEncoding = TaggedObject
    { tagFieldName = "command_byte"
    , contentsFieldName = "arguments" }}

instance ToJSON   a => ToJSON   (SCPSeg e ps          a) where
    toJSON     = genericToJSON     jcSCPSeg
    toEncoding = genericToEncoding jcSCPSeg
instance FromJSON a => FromJSON (SCPSeg e 'Unenforced a) where
    parseJSON  = genericParseJSON  jcSCPSeg

data SCPSeg05Textbox e bs = SCPSeg05Textbox'
  { scpSeg05TextboxSpeakerUnkCharID :: W8 e
  -- ^ Unknown identifier. Game SCP parser uses it (places in memory, uses in a
  --   call to set a value). Appears to be a "character ID" -- stronger than a
  --   speaker ID, which can change between the same character to display their
  --   name differently (e.g. if the player hasn't been introduced).
  --
  --   @0@ is shared by multiple non-important characters (e.g. random unnamed
  --   university kids).

  , scpSeg05TextboxSpeakerID :: W32 e
  -- ^ Speaker ID. Selects the textbox name graphic and name used in backlog.
  --   @0@ is apparently invalid (points to an empty string in one of (?) the
  --   speaker ID arrays).

  , scpSeg05TextboxText :: bs
  -- ^ Textbox text. (Warning: Game imposes harsh length limitations.)

  , scpSeg05TextboxVoiceLine :: bs
  -- ^ No voice line is allowed, and indicated by the empty string.
  --
  -- There's also apparently a check for Banri's thought lines, accomplished via
  -- a string search for the @（@ character. If it's found, the voice line isn't
  -- copied. However, regardless of if that's correct or not, copying the empty
  -- string accomplishes pretty much the same thing.

  , scpSeg05TextboxCounter :: W32 e
  -- ^ Some sort of counter used throughout all SCPs.
  --
  --   TODO: May be globally unique. In which case, we want to determine the
  --   "canonical" route through the SCPs, to potentially recalculate them.

  } deriving stock (Generic, Typeable, Show, Foldable, Eq, Functor, Traversable)

jcSCPSeg05Textbox :: Options
jcSCPSeg05Textbox =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpSeg05Textbox" :: String)

instance ToJSON   a => ToJSON   (SCPSeg05Textbox e a) where
    toJSON     = genericToJSON     jcSCPSeg05Textbox
    toEncoding = genericToEncoding jcSCPSeg05Textbox
instance FromJSON a => FromJSON (SCPSeg05Textbox e a) where
    parseJSON  = genericParseJSON  jcSCPSeg05Textbox

-- | A weird dynamic bit used in 0A and 2B.
--
-- Other dynamic parts are simple, and done in line. This data is read and
-- handled by a dedicated function, used exactly twice - once in 0A (simple) and
-- once in 2B (also stores a bunch of other data).
type SCPSeg0ADyn e ps bs = WR ps ('Pascal 'I1 e) (WR ps ('Pascal 'I1) [(bs, W32 e)])

-- | Encode an SCP to pretty YAML.
--
-- We use a convenient trick provided by Snoyman's YAML library to allow even
-- prettier YAML, where we force field order via overriding string comparison.
encodeYamlPretty :: ToJSON a => SCP e ps a -> BS.ByteString
encodeYamlPretty = YamlPretty.encodePretty yamlPrettyCfg
  where
    yamlPrettyCfg = YamlPretty.setConfCompare cmp $ YamlPretty.setConfDropNull True YamlPretty.defConfig
    cmp "command_byte" _ = LT
    cmp _ "command_byte" = GT
    cmp k1 k2 = Prelude.compare k1 k2

---

scpUnenforce :: SCP e 'Enforced bs -> SCP e 'Unenforced bs
scpUnenforce = fmap scpSegUnenforce

scpSegUnenforce :: SCPSeg e 'Enforced bs -> SCPSeg e 'Unenforced bs
scpSegUnenforce = \case
  SCPSeg00 -> SCPSeg00
  SCPSeg01BG bs w1 w2 -> SCPSeg01BG bs w1 w2
{-
  | SCPSeg02SFX bs (W8 e)
  | SCPSeg03 (W8 e) bs (W8 e)
  | SCPSeg04 (W8 e) (W8 e)
  | SCPSeg05Textbox (SCPSeg05Textbox e bs)
  | SCPSeg07SCP bs
  | SCPSeg08
  | SCPSeg09Choice (W8 e) (WR ps ('Pascal 'I1 e) [(bs, W32 e)])
  | SCPSeg0A (SCPSeg0ADyn e ps bs)
  | SCPSeg0B (W8 e) (W8 e)
  | SCPSeg0CFlag (W8 e) (W8 e)
  | SCPSeg0D (W8 e)
  | SCPSeg0E (W8 e)
  | SCPSeg0F
  | SCPSeg10 (W8 e) (W8 e) (W8 e)
  | SCPSeg11EventCG bs
  | SCPSeg12
  | SCPSeg13 (W8 e) bs (W8 e) (W32 e)
  | SCPSeg14 (W8 e)
  | SCPSeg15
  | SCPSeg16Wadai
  | SCPSeg17 (W8 e) (W8 e)
  | SCPSeg18 (W8 e) (W8 e)
  | SCPSeg19 (W8 e) (W8 e)
  | SCPSeg1D
  | SCPSeg1E (W8 e)
  | SCPSeg1FDelay
  | SCPSeg20 (W8 e)
  | SCPSeg21
  | SCPSeg22 bs (WR ps ('Pascal 'I1 e) [(bs, W32 e)])
  | SCPSeg23SFX
  | SCPSeg24 bs
  | SCPSeg25
  | SCPSeg26
  | SCPSeg27 bs (W8 e) (W8 e)
  | SCPSeg28 bs (W8 e) (W8 e)
  | SCPSeg29 bs (W8 e) (W8 e)
  | SCPSeg2A (W8 e) (W8 e)
  | SCPSeg2B (W8 e) (W8 e) (W8 e) bs (SCPSeg0ADyn e ps bs)
  | SCPSeg2CMap
  | SCPSeg2D bs (W8 e) (W8 e)
  | SCPSeg2E (W8 e) (W8 e) (W32 e) (W32 e)
  | SCPSeg2F (W8 e)
  | SCPSeg30 (W8 e)
  | SCPSeg31
  | SCPSeg32 (W32 e) (W8 e)
  | SCPSeg33 (W8 e) (W8 e)
  | SCPSeg34 (W32 e)
  | SCPSeg35 bs
  | SCPSeg36 (W8 e) (W32 e)
  | SCPSeg37 (W8 e) (W32 e)
  | SCPSeg38 (W8 e) (W32 e)
  | SCPSeg39
  | SCPSeg3A (W8 e) (W8 e)
  | SCPSeg3B (W8 e)
  | SCPSeg3C (W8 e) (W8 e)
  | SCPSeg3D
  | SCPSeg3E (W8 e) (W8 e)
  | SCPSeg3F (W8 e) (W32 e) (W32 e)
  | SCPSeg40 (W8 e)
  | SCPSeg41 (W8 e) (W32 e) (W32 e)
  | SCPSeg42 (W8 e)
  | SCPSeg43SFX bs
  | SCPSeg44SFX bs
  | SCPSeg45SFX bs (W8 e)
  | SCPSeg46 (W8 e) (W8 e)
  | SCPSeg47 (W8 e) (W8 e)
  | SCPSeg48
  | SCPSeg49
  | SCPSeg4A
  | SCPSeg4B
  | SCPSeg4C bs
  | SCPSeg4D
  | SCPSeg4E
  | SCPSeg4F bs (W8 e) (W8 e)
  | SCPSeg50 bs (W8 e) (W8 e)
  | SCPSeg51 bs (W8 e) (W8 e)
  | SCPSeg52 bs
  | SCPSeg53 bs (W8 e) (W8 e)
  | SCPSeg54 (W8 e) (W8 e)
  | SCPSeg55 (W8 e) (W8 e)
  | SCPSeg56 (W8 e) (W8 e)
  | SCPSeg57 (W8 e) (W8 e)
  | SCPSeg58 (W8 e) (W8 e)
  | SCPSeg59 (W8 e)
  | SCPSeg5A (W32 e) (W8 e)
  | SCPSeg5B (W32 e)
  | SCPSeg5C (W8 e)
  | SCPSeg5D (W32 e)
  | SCPSeg5E (W32 e)
  | SCPSeg5F (W32 e)
  | SCPSeg60 (W32 e)
  | SCPSeg61 (W8 e) (W8 e)
  | SCPSeg62 (W8 e)
  | SCPSeg63
  | SCPSeg64
  | SCPSeg65Trophy
  | SCPSeg66
  | SCPSeg67
  | SCPSeg68
  | SCPSeg69
  | SCPSeg6A (W8 e)
  | SCPSeg6B (W8 e)
  | SCPSeg6CWipe (W8 e) (W32 e) (W32 e) (W32 e)
  | SCPSeg6DWipe (W8 e) (W32 e) (W32 e) (W32 e)
  | SCPSeg6E
  | SCPSeg6F
  | SCPSeg70 (W8 e) (W8 e)
  | SCPSeg71 (W8 e) (W8 e)
  | SCPSeg72
  | SCPSeg73Kyoro (W8 e) (W32 e)
  | SCPSeg74
  | SCPSeg75 (W8 e)
  | SCPSeg76
  | SCPSeg77SCP (W8 e)
-}

scpSegEnforce
    :: forall (rep :: Binrep.Types.ByteString.Rep) e bs bs'
    .  ( bs' ~ Refined rep bs )
    => SCPSeg e 'Unenforced bs'
    -> Either RefineException (SCPSeg e 'Enforced bs')
scpSegEnforce = \case
  SCPSeg00 -> Right $ SCPSeg00
  SCPSeg01BG bs w1 w2 -> Right $ SCPSeg01BG bs w1 w2
{-
  | SCPSeg02SFX bs (W8 e)
  | SCPSeg03 (W8 e) bs (W8 e)
  | SCPSeg04 (W8 e) (W8 e)
  | SCPSeg05Textbox (SCPSeg05Textbox e bs)
  | SCPSeg07SCP bs
  | SCPSeg08
  | SCPSeg09Choice (W8 e) (WR ps ('Pascal 'I1 e) [(bs, W32 e)])
  | SCPSeg0A (SCPSeg0ADyn e ps bs)
  | SCPSeg0B (W8 e) (W8 e)
  | SCPSeg0CFlag (W8 e) (W8 e)
  | SCPSeg0D (W8 e)
  | SCPSeg0E (W8 e)
  | SCPSeg0F
  | SCPSeg10 (W8 e) (W8 e) (W8 e)
  | SCPSeg11EventCG bs
  | SCPSeg12
  | SCPSeg13 (W8 e) bs (W8 e) (W32 e)
  | SCPSeg14 (W8 e)
  | SCPSeg15
  | SCPSeg16Wadai
  | SCPSeg17 (W8 e) (W8 e)
  | SCPSeg18 (W8 e) (W8 e)
  | SCPSeg19 (W8 e) (W8 e)
  | SCPSeg1D
  | SCPSeg1E (W8 e)
  | SCPSeg1FDelay
  | SCPSeg20 (W8 e)
  | SCPSeg21
  | SCPSeg22 bs (WR ps ('Pascal 'I1 e) [(bs, W32 e)])
  | SCPSeg23SFX
  | SCPSeg24 bs
  | SCPSeg25
  | SCPSeg26
  | SCPSeg27 bs (W8 e) (W8 e)
  | SCPSeg28 bs (W8 e) (W8 e)
  | SCPSeg29 bs (W8 e) (W8 e)
  | SCPSeg2A (W8 e) (W8 e)
  | SCPSeg2B (W8 e) (W8 e) (W8 e) bs (SCPSeg0ADyn e ps bs)
  | SCPSeg2CMap
  | SCPSeg2D bs (W8 e) (W8 e)
  | SCPSeg2E (W8 e) (W8 e) (W32 e) (W32 e)
  | SCPSeg2F (W8 e)
  | SCPSeg30 (W8 e)
  | SCPSeg31
  | SCPSeg32 (W32 e) (W8 e)
  | SCPSeg33 (W8 e) (W8 e)
  | SCPSeg34 (W32 e)
  | SCPSeg35 bs
  | SCPSeg36 (W8 e) (W32 e)
  | SCPSeg37 (W8 e) (W32 e)
  | SCPSeg38 (W8 e) (W32 e)
  | SCPSeg39
  | SCPSeg3A (W8 e) (W8 e)
  | SCPSeg3B (W8 e)
  | SCPSeg3C (W8 e) (W8 e)
  | SCPSeg3D
  | SCPSeg3E (W8 e) (W8 e)
  | SCPSeg3F (W8 e) (W32 e) (W32 e)
  | SCPSeg40 (W8 e)
  | SCPSeg41 (W8 e) (W32 e) (W32 e)
  | SCPSeg42 (W8 e)
  | SCPSeg43SFX bs
  | SCPSeg44SFX bs
  | SCPSeg45SFX bs (W8 e)
  | SCPSeg46 (W8 e) (W8 e)
  | SCPSeg47 (W8 e) (W8 e)
  | SCPSeg48
  | SCPSeg49
  | SCPSeg4A
  | SCPSeg4B
  | SCPSeg4C bs
  | SCPSeg4D
  | SCPSeg4E
  | SCPSeg4F bs (W8 e) (W8 e)
  | SCPSeg50 bs (W8 e) (W8 e)
  | SCPSeg51 bs (W8 e) (W8 e)
  | SCPSeg52 bs
  | SCPSeg53 bs (W8 e) (W8 e)
  | SCPSeg54 (W8 e) (W8 e)
  | SCPSeg55 (W8 e) (W8 e)
  | SCPSeg56 (W8 e) (W8 e)
  | SCPSeg57 (W8 e) (W8 e)
  | SCPSeg58 (W8 e) (W8 e)
  | SCPSeg59 (W8 e)
  | SCPSeg5A (W32 e) (W8 e)
  | SCPSeg5B (W32 e)
  | SCPSeg5C (W8 e)
  | SCPSeg5D (W32 e)
  | SCPSeg5E (W32 e)
  | SCPSeg5F (W32 e)
  | SCPSeg60 (W32 e)
  | SCPSeg61 (W8 e) (W8 e)
  | SCPSeg62 (W8 e)
  | SCPSeg63
  | SCPSeg64
  | SCPSeg65Trophy
  | SCPSeg66
  | SCPSeg67
  | SCPSeg68
  | SCPSeg69
  | SCPSeg6A (W8 e)
  | SCPSeg6B (W8 e)
  | SCPSeg6CWipe (W8 e) (W32 e) (W32 e) (W32 e)
  | SCPSeg6DWipe (W8 e) (W32 e) (W32 e) (W32 e)
  | SCPSeg6E
  | SCPSeg6F
  | SCPSeg70 (W8 e) (W8 e)
  | SCPSeg71 (W8 e) (W8 e)
  | SCPSeg72
  | SCPSeg73Kyoro (W8 e) (W32 e)
  | SCPSeg74
  | SCPSeg75 (W8 e)
  | SCPSeg76
  | SCPSeg77SCP (W8 e)
-}
