module GTVM.SCP
  ( SCPSegment(..)
  , SCPSeg05Textbox(..)
  , SCPXText(..)
  , Speaker(..)
  , speakerInfo
  ) where

import           Data.Word
import           GHC.Generics
import           Data.Aeson
import           Data.Text      (Text)

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

  , scpSeg05TextboxCounter :: Word32
  -- ^ Some sort of counter used throughout all SCPs.
  --
  --   TODO: May be globally unique. In which case, we want to determine the
  --   "canonical" route through the SCPs, to potentially recalculate them.

  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

jcSCPSeg05Textbox :: Options
jcSCPSeg05Textbox = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length "scpSeg05Textbox") }

instance ToJSON   a => ToJSON   (SCPSeg05Textbox a) where
    toJSON     = genericToJSON     jcSCPSeg05Textbox
    toEncoding = genericToEncoding jcSCPSeg05Textbox
instance FromJSON a => FromJSON (SCPSeg05Textbox a) where
    parseJSON  = genericParseJSON  jcSCPSeg05Textbox

data SCPXText = SCPXText'
  { scpXTextText      :: Text
  , scpXTextSpeaker   :: Speaker
  , scpXTextVoiceLine :: Maybe Text
  } deriving (Eq, Show, Generic)

jcSCPXText :: Options
jcSCPXText = defaultOptions
  { fieldLabelModifier = camelTo2 '_' . drop (length "scpXText") }

instance ToJSON   SCPXText where
    toJSON     = genericToJSON     jcSCPXText
    toEncoding = genericToEncoding jcSCPXText
instance FromJSON SCPXText where
    parseJSON  = genericParseJSON  jcSCPXText

-- QM = question mark (at end)
-- Unk = unknown (written as full ???)
data Speaker
  = SpeakerBanri
  | SpeakerSao
  | SpeakerReikonBanriUnk
  | SpeakerReikonBanriQM
  | SpeakerReikonBanri
    deriving (Eq, Show, Generic)

jcSpeaker :: Options
jcSpeaker = defaultOptions { constructorTagModifier = camelTo2 ' ' . drop 7 }

instance ToJSON   Speaker where
    toJSON     = genericToJSON     jcSpeaker
    toEncoding = genericToEncoding jcSpeaker
instance FromJSON Speaker where
    parseJSON  = genericParseJSON  jcSpeaker

speakerInfo :: Speaker -> (Word8, Word32)
speakerInfo = \case
  SpeakerBanri          -> (5, 5)
  SpeakerSao            -> (8, 8)
  SpeakerReikonBanriUnk -> (16, 17)
  SpeakerReikonBanriQM  -> (16, 18)
  SpeakerReikonBanri    -> (16, 16)

-- TODO: Annotate data with meaning. We shouldn't hardcode this into the AST
-- during parsing like `SCPStr bs UserFacing`, but how to do it post-parse?

-- | A standalone segment of an SCP file. Essentially tokens from a byte lexer.
--
-- Generalized over the bytestring representation (for easier coercing between
-- 'ByteString' and 'Text').
data SCPSegment bs
  = SCPSeg00
  | SCPSeg01BG bs Word8 Word8
  | SCPSeg02SFX bs Word8
  | SCPSeg03 Word8 bs Word8
  | SCPSeg04 Word8 Word8
  | SCPSeg05Textbox (SCPSeg05Textbox bs)
  -- no 0x06
  | SCPSeg07SCP bs
  | SCPSeg08
  | SCPSeg09 Word8 [(bs, Word32)]
  | SCPSeg0A Word8 Word8 Word32 Word32 Word32
  -- ^ TODO: decomp code a bit confusing for this command, be aware
  | SCPSeg0B Word8 Word8
  | SCPSeg0CFlag Word8 Word8
  | SCPSeg0D Word8
  | SCPSeg0E Word8
  | SCPSeg0F
  | SCPSeg10 Word8 Word8 Word8
  | SCPSeg11EventCG bs
  | SCPSeg12
  | SCPSeg13 Word8 bs Word8 Word32
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
  | SCPSeg23SFX
  | SCPSeg24 bs
  | SCPSeg25
  | SCPSeg26
  | SCPSeg27 bs Word8 Word8
  | SCPSeg28 bs Word8 Word8
  | SCPSeg29 bs Word8 Word8
  | SCPSeg2A Word8 Word8
  | SCPSeg2B Word8 Word8 Word8 bs Word8
  | SCPSeg2CMap
  | SCPSeg2D bs Word8 Word8
  | SCPSeg2E Word8 Word8 Word32 Word32
  | SCPSeg2F Word8
  | SCPSeg30 Word8
  | SCPSeg31
  | SCPSeg32 Word32 Word8
  | SCPSeg33 Word8 Word8
  | SCPSeg34 Word32
  | SCPSeg35 bs
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
  | SCPSeg4D
  | SCPSeg4E
  | SCPSeg4F bs Word8 Word8
  | SCPSeg50 bs Word8 Word8
  | SCPSeg51 bs Word8 Word8
  | SCPSeg52 bs
  | SCPSeg53 bs Word8 Word8
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
    deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

-- | Lexed SCP JSON en/decoding config.
--
-- JSON tag names are derived by dropping the @SCPSeg@ and taking just the 2 hex
-- digits. (The suffix is just internal best-guesses at what they mean, subject
-- to change.)
--
-- Also, "tag" -> "command_byte", "contents" -> "arguments".
configJSON :: Options
configJSON = defaultOptions
  { constructorTagModifier = take 2 . drop 6
  , sumEncoding = defaultTaggedObject
    { tagFieldName = "command_byte"
    , contentsFieldName = "arguments" }}

instance ToJSON   a => ToJSON   (SCPSegment a) where
    toJSON     = genericToJSON     configJSON
    toEncoding = genericToEncoding configJSON
instance FromJSON a => FromJSON (SCPSegment a) where
    parseJSON  = genericParseJSON  configJSON
