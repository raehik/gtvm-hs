{-# LANGUAGE OverloadedStrings #-}

module Tool.Flowchart where

import Common.Config
import Common.CLIOptions
import Common.Util
import GTVM.Flowchart
import Binrep
import Binrep.Types.ByteString
import Binrep.Types.Text
import Refined
import Refined.WithRefine
import Options.Applicative
import GHC.Generics ( Generic )
import Control.Monad.IO.Class
import Data.Text ( Text )
import Data.Yaml.Pretty qualified
import GTVM.Common.IO ( badParseYAML )
import Data.ByteString qualified as BS

data CfgEncode = CfgEncode
  { cfgEncodeStreamIn  :: Stream 'StreamIn  "YAML flowchart"
  , cfgEncodeStreamOut :: Stream 'StreamOut "binary flowchart"
  , cfgEncodePrintBin  :: PrintBin
  } deriving (Eq, Show, Generic)

data CfgDecode = CfgDecode
  { cfgDecodeStreamIn  :: Stream 'StreamIn  "binary flowchart"
  , cfgDecodeStreamOut :: Stream 'StreamOut "YAML flowchart"
  } deriving (Eq, Show, Generic)

parseCLIOptsEncode :: Parser CfgEncode
parseCLIOptsEncode = CfgEncode <$> pStreamIn <*> pStreamOut <*> pPrintBin

parseCLIOptsDecode :: Parser CfgDecode
parseCLIOptsDecode = CfgDecode <$> pStreamIn <*> pStreamOut

runEncode :: MonadIO m => CfgEncode -> m ()
runEncode cfg = do
    fcBsYaml <- readStreamBytes $ cfgEncodeStreamIn cfg
    fcText  <- badParseYAML @(Flowchart 'Unenforced (Refined 'UTF8 Text)) fcBsYaml
    fcBin   <- liftErr show $ fcBytesRefine @'C $ fcTextToBytes fcText
    fcBin'  <- liftErr show $ enforceFlowchart fcBin
    let fcBsBin = binEncode fcBin'
    writeStreamBin (cfgEncodePrintBin cfg) (cfgEncodeStreamOut cfg) fcBsBin

runDecode :: MonadIO m => CfgDecode -> m ()
runDecode cfg = do
    fcBsBin <- readStreamBytes $ cfgDecodeStreamIn cfg
    fcBin   <- liftErr id   $ binDecode @(Flowchart 'Enforced (Refined 'C BS.ByteString)) fcBsBin
    fcText  <- liftErr show $ fcBytesToText @'UTF8 @'C $ unenforceFlowchart fcBin
    let fcText'  = fcMap unrefine fcText
        fcBsYaml = Data.Yaml.Pretty.encodePretty ycFCTL fcText'
    writeStreamTextualBytes (cfgDecodeStreamOut cfg) fcBsYaml

-- | Silly pretty config to get my preferred layout easily.
--
-- Looks silly, but gets the job done very smoothly. Snoyman's yaml library is
-- based for exposing this.
ycFCTL :: Data.Yaml.Pretty.Config
ycFCTL = Data.Yaml.Pretty.setConfCompare f $ Data.Yaml.Pretty.defConfig
  where f "entries" _ = GT
        f _ "entries" = LT
        f "script" _ = LT
        f _ "script" = GT
        f "index" _ = GT
        f _ "index" = LT
        f s1 s2 = compare s1 s2
