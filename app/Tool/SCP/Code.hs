module Tool.SCP.Code where

import Common.Config
import Common.CLIOptions
import Common.Util
import Common.IO ( badParseYAML )
import Tool.SCP.Common ( scpPrettyYamlCfg )

import Options.Applicative
import GHC.Generics
import Control.Monad.IO.Class
import GTVM.SCP
import Binrep
import Binrep.Type.NullTerminated
import Binrep.Type.Text ( encodeToRep, decode, AsText, Utf8 )
import Data.Yaml.Pretty qualified as Yaml.Pretty
import Data.ByteString qualified as B
import Rerefined
import Strongweak
import Data.Text ( Text )
import Data.Aeson qualified as Aeson

type SCPBin  = SCP Strong (NullTerminated B.ByteString)
type SCPText = SCP 'Weak (AsText Utf8)

-- TODO ugly workaround because IDK
instance Aeson.FromJSON (Refined Utf8 Text) where
    parseJSON x = unsafeRefine <$> Aeson.parseJSON x
instance Aeson.ToJSON   (Refined Utf8 Text) where
    toJSON     = Aeson.toJSON     . unrefine
    toEncoding = Aeson.toEncoding . unrefine

data CfgEncode = CfgEncode
  { cfgEncodeStreamIn  :: Stream 'StreamIn  "YAML SCP"
  , cfgEncodeStreamOut :: Stream 'StreamOut "binary SCP"
  , cfgEncodePrintBin  :: PrintBin
  } deriving (Eq, Show, Generic)

data CfgDecode = CfgDecode
  { cfgDecodeStreamIn  :: Stream 'StreamIn  "binary SCP"
  , cfgDecodeStreamOut :: Stream 'StreamOut "YAML SCP"
  } deriving (Eq, Show, Generic)

parseCLIOptsEncode :: Parser CfgEncode
parseCLIOptsEncode = CfgEncode <$> pStreamIn <*> pStreamOut <*> pPrintBin

parseCLIOptsDecode :: Parser CfgDecode
parseCLIOptsDecode = CfgDecode <$> pStreamIn <*> pStreamOut

runEncode :: MonadIO m => CfgEncode -> m ()
runEncode cfg = do
    scpYAMLBs <- readStreamBytes $ cfgEncodeStreamIn cfg
    scpYAML <- badParseYAML @SCPText scpYAMLBs
    scpBin' <- liftErr show $ scpTraverse encodeToRep scpYAML
    scpBin :: SCPBin <- liftStrengthen scpBin'
    let scpBinBs = runPut scpBin
    writeStreamBin (cfgEncodePrintBin cfg) (cfgEncodeStreamOut cfg) scpBinBs

runDecode :: (MonadFail m, MonadIO m) => CfgDecode -> m ()
runDecode cfg = do
    scpBinBs <- readStreamBytes $ cfgDecodeStreamIn cfg
    (scpBin, bs) <- liftErr show $ runGet @SCPBin scpBinBs
    if not (B.null bs) then fail "dangling bytes"
    else do
        scpText :: SCPText <- liftErr id $ scpTraverse decode $ scpFmap unrefine $ weaken scpBin
        let scpYAMLBs = Yaml.Pretty.encodePretty scpPrettyYamlCfg scpText
        writeStreamTextualBytes (cfgDecodeStreamOut cfg) scpYAMLBs
