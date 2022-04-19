{-# LANGUAGE OverloadedStrings #-}

module GTVM.SCP
  ( SCP
  , SCPSeg(..)
  , SCPSeg05Textbox(..)
  , SCPSeg0ADyn(..)

  , bsToText
  , textToBs

  , encodeYamlPretty
  ) where

import Data.Word
import GHC.Generics
import GTVM.Common.Json
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Text ( Text )

import Data.Aeson
import Data.Yaml.Pretty qualified as YamlPretty

-- | A SCP file.
--
-- SCPs are ordered lists of individual segments (commands, macros, whatever).
-- Segments are identified by a single prefix byte. Once the prefix byte is
-- recognized, the segment is parsed accordingly, then the next prefix byte is
-- read, and so on until it finds EOF instead of a prefix byte.
type SCP bs = [SCPSeg bs]

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
data SCPSeg bs
  = SCPSeg00
  | SCPSeg01BG bs Word8 Word8
  | SCPSeg02SFX bs Word8
  | SCPSeg03 Word8 bs Word8
  | SCPSeg04 Word8 Word8

  | SCPSeg05Textbox (SCPSeg05Textbox bs)
  -- ^ Includes player-facing text.

  -- no 0x06

  | SCPSeg07SCP bs
  | SCPSeg08

  | SCPSeg09Choice Word8 [(bs, Word32)]
  -- ^ Includes player-facing text. Choice selection. The 'Word32's appear to be
  --   the same counter as textboxes!
  --
  -- The 'Word8' seems to be an file-unique identifier for the choice selection.
  -- SCP files with multiple choices have 0, 1, 2 etc. in ascending order.

  | SCPSeg0A SCPSeg0ADyn

  | SCPSeg0B Word8 Word8
  -- ^ Appears to indicate where a given choice jumps to.
  --
  -- First 'Word8' appears to specify which choice selection it relates to.
  -- Second 'Word8' appears to be the choice index in that choice selection
  -- (usually 0, 1). They may be highly separated, in cases where a choice
  -- changes lots of dialog.

  | SCPSeg0CFlag Word8 Word8
  | SCPSeg0D Word8
  | SCPSeg0E Word8
  | SCPSeg0F
  | SCPSeg10 Word8 Word8 Word8
  | SCPSeg11EventCG bs
  | SCPSeg12

  | SCPSeg13 Word8 bs Word8 Word32
  -- ^ Possibly player-facing text. Registers words for the feast
  --   Danganronpa-style minigame, but using indices which correspond to
  --   textures with the text on. The usage of the text here is unknown.

  | SCPSeg14 Word8
  | SCPSeg15
  | SCPSeg16Wadai
  | SCPSeg17 Word8 Word8
  | SCPSeg18 Word8 Word8
  | SCPSeg19 Word8 Word8
  -- no 0x1A
  -- no 0x1B
  -- no 0x1C
  | SCPSeg1D
  | SCPSeg1E Word8
  | SCPSeg1FDelay
  | SCPSeg20 Word8
  | SCPSeg21

  | SCPSeg22 bs [(bs, Word32)]
  -- ^ Includes player-facing text. Choice, plus an extra string. Seem to be
  --   used in the conversation events.

  | SCPSeg23SFX

  | SCPSeg24 bs
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg25
  | SCPSeg26

  | SCPSeg27 bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg28 bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg29 bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg2A Word8 Word8

  | SCPSeg2B Word8 Word8 Word8 bs SCPSeg0ADyn

  | SCPSeg2CMap

  | SCPSeg2D bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg2E Word8 Word8 Word32 Word32
  | SCPSeg2F Word8
  | SCPSeg30 Word8
  | SCPSeg31
  | SCPSeg32 Word32 Word8
  | SCPSeg33 Word8 Word8
  | SCPSeg34 Word32

  | SCPSeg35 bs
  -- ^ Likely player-facing. Kinda sounds like it's a type of Banri choice.

  | SCPSeg36 Word8 Word32
  | SCPSeg37 Word8 Word32
  | SCPSeg38 Word8 Word32
  | SCPSeg39
  | SCPSeg3A Word8 Word8
  | SCPSeg3B Word8
  | SCPSeg3C Word8 Word8
  | SCPSeg3D
  | SCPSeg3E Word8 Word8
  | SCPSeg3F Word8 Word32 Word32
  | SCPSeg40 Word8
  | SCPSeg41 Word8 Word32 Word32
  | SCPSeg42 Word8

  -- these don't appear very SFXy in code, but did in data
  | SCPSeg43SFX bs
  | SCPSeg44SFX bs
  | SCPSeg45SFX bs Word8

  | SCPSeg46 Word8 Word8
  | SCPSeg47 Word8 Word8
  | SCPSeg48
  | SCPSeg49
  | SCPSeg4A
  | SCPSeg4B

  | SCPSeg4C bs
  -- Unknown. Appears unused.

  | SCPSeg4D
  | SCPSeg4E

  | SCPSeg4F bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg50 bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg51 bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg52 bs
  -- ^ Text not user-facing. Refers to a @sound/voice@ file - all the uses I've
  --   seen are girls telling you 飲んで飲んで！

  | SCPSeg53 bs Word8 Word8
  -- ^ Unknown. Text seems to correspond to MDL files in the @r2d@ directory.

  | SCPSeg54 Word8 Word8
  | SCPSeg55 Word8 Word8
  | SCPSeg56 Word8 Word8
  | SCPSeg57 Word8 Word8
  | SCPSeg58 Word8 Word8
  | SCPSeg59 Word8
  | SCPSeg5A Word32 Word8
  | SCPSeg5B Word32
  | SCPSeg5C Word8
  | SCPSeg5D Word32
  | SCPSeg5E Word32
  | SCPSeg5F Word32
  | SCPSeg60 Word32
  | SCPSeg61 Word8 Word8
  | SCPSeg62 Word8 -- 0x01 <= w8 <= 0x11, no default
  | SCPSeg63
  | SCPSeg64
  | SCPSeg65Trophy
  | SCPSeg66
  | SCPSeg67
  | SCPSeg68
  | SCPSeg69
  | SCPSeg6A Word8
  | SCPSeg6B Word8
  | SCPSeg6CWipe Word8 Word32 Word32 Word32
  | SCPSeg6DWipe Word8 Word32 Word32 Word32
  | SCPSeg6E
  | SCPSeg6F
  | SCPSeg70 Word8 Word8
  | SCPSeg71 Word8 Word8
  | SCPSeg72
  | SCPSeg73Kyoro Word8 Word32 -- 0x01 <= w8 <= 0x06, with default
  | SCPSeg74
  | SCPSeg75 Word8
  | SCPSeg76
  | SCPSeg77SCP Word8
    deriving stock (Generic, Eq, Show, Functor, Foldable, Traversable)

-- | SCP segment JSON en/decoding config.
--
-- For selecting the constructor, only the 2 hex digits are used.
jcSCPSeg :: Options
jcSCPSeg = defaultOptions
  { constructorTagModifier = take 2 . drop 6
  , sumEncoding = TaggedObject
    { tagFieldName = "command_byte"
    , contentsFieldName = "arguments" }}

instance ToJSON   a => ToJSON   (SCPSeg a) where
    toJSON     = genericToJSON     jcSCPSeg
    toEncoding = genericToEncoding jcSCPSeg
instance FromJSON a => FromJSON (SCPSeg a) where
    parseJSON  = genericParseJSON  jcSCPSeg

bsToText :: SCP BS.ByteString -> Either Text.UnicodeException (SCP Text)
bsToText = traverse (traverse Text.decodeUtf8')

textToBs :: SCP Text -> SCP BS.ByteString
textToBs = map (fmap Text.encodeUtf8)

data SCPSeg05Textbox bs = SCPSeg05Textbox'
  { scpSeg05TextboxSpeakerUnkCharID :: Word8
  -- ^ Unknown identifier. Game SCP parser uses it (places in memory, uses in a
  --   call to set a value). Appears to be a "character ID" -- stronger than a
  --   speaker ID, which can change between the same character to display their
  --   name differently (e.g. if the player hasn't been introduced).
  --
  --   @0@ is shared by multiple non-important characters (e.g. random unnamed
  --   university kids).

  , scpSeg05TextboxSpeakerID :: Word32
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

  , scpSeg05TextboxCounter :: Word32
  -- ^ Some sort of counter used throughout all SCPs.
  --
  --   TODO: May be globally unique. In which case, we want to determine the
  --   "canonical" route through the SCPs, to potentially recalculate them.

  } deriving stock (Generic, Eq, Show, Functor, Foldable, Traversable)

jcSCPSeg05Textbox :: Options
jcSCPSeg05Textbox =
    jsonCfgSepUnderscoreDropN $ fromIntegral $ length ("scpSeg05Textbox" :: String)

instance ToJSON   a => ToJSON   (SCPSeg05Textbox a) where
    toJSON     = genericToJSON     jcSCPSeg05Textbox
    toEncoding = genericToEncoding jcSCPSeg05Textbox
instance FromJSON a => FromJSON (SCPSeg05Textbox a) where
    parseJSON  = genericParseJSON  jcSCPSeg05Textbox

-- | A weird dynamic bit used in 0A and 2B.
--
-- Other dynamic parts are simple, and done in line. This data is read and
-- handled by a dedicated function, used exactly twice - once in 0A (simple) and
-- once in 2B (also stores a bunch of other data).
newtype SCPSeg0ADyn = SCPSeg0ADyn
  { scpSeg0ADynData :: [[Word32]]
  } deriving stock (Generic, Eq, Show)
    deriving (ToJSON, FromJSON) via [[Word32]]

-- | Encode an SCP to pretty YAML.
--
-- We use a convenient trick provided by Snoyman's YAML library to allow even
-- prettier YAML, where we force field order via overriding string comparison.
encodeYamlPretty :: ToJSON a => SCP a -> BS.ByteString
encodeYamlPretty = YamlPretty.encodePretty yamlPrettyCfg
  where
    yamlPrettyCfg = YamlPretty.setConfCompare cmp $ YamlPretty.setConfDropNull True YamlPretty.defConfig
    cmp "command_byte" _ = LT
    cmp _ "command_byte" = GT
    cmp k1 k2 = Prelude.compare k1 k2
