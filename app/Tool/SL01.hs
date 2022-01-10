module Tool.SL01 where

import Common.Config
import Common.CLIOptions
import Common.Util
import Options.Applicative
import GHC.Generics
import Control.Monad.IO.Class
import GTVM.Assorted.SL01 qualified as GAS
import GTVM.Common.Binary qualified as GCB
import GTVM.Common.Binary.Parse qualified as GCBP

data CfgCompress = CfgCompress
  { cfgCompressStreamIn  :: Stream 'StreamIn  "data"
  , cfgCompressStreamOut :: Stream 'StreamOut "compressed data"
  , cfgCompressPrintBin  :: PrintBin
  } deriving (Eq, Show, Generic)

data CfgDecompress = CfgDecompress
  { cfgDecompressStreamIn  :: Stream 'StreamIn  "compressed data"
  , cfgDecompressStreamOut :: Stream 'StreamOut "data"
  , cfgDecompressPrintBin  :: PrintBin
  } deriving (Eq, Show, Generic)

parseCLIOptsCompress :: Parser CfgCompress
parseCLIOptsCompress = CfgCompress <$> pStreamIn <*> pStreamOut <*> pPrintBin

parseCLIOptsDecompress :: Parser CfgDecompress
parseCLIOptsDecompress = CfgDecompress <$> pStreamIn <*> pStreamOut <*> pPrintBin

runCompress :: MonadIO m => CfgCompress -> m ()
runCompress cfg = do
    dataBs <- readStreamBytes $ cfgCompressStreamIn cfg
    let compressed = GAS.compress dataBs
        compressedBs = GAS.sSL01 compressed GCB.binCfgSCP
    writeStreamBin (cfgCompressPrintBin cfg) (cfgCompressStreamOut cfg) compressedBs

runDecompress :: MonadIO m => CfgDecompress -> m ()
runDecompress cfg = do
    compressed <- badParseStream parse $ cfgDecompressStreamIn cfg
    let dataBs = GAS.decompress compressed
    writeStreamBin (cfgDecompressPrintBin cfg) (cfgDecompressStreamOut cfg) dataBs
  where
    parse fp bs = GCBP.parseBin GAS.pSL01 GCB.binCfgSCP fp bs
