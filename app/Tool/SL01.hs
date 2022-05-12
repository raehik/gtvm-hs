module Tool.SL01 where

import Common.Config
import Common.CLIOptions
import Common.Util
import Options.Applicative
import GHC.Generics
import Control.Monad.IO.Class
import GTVM.Assorted.SL01 qualified as SL01

import Binrep

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
    bs <- readStreamBytes $ cfgCompressStreamIn cfg
    sl01 <- liftErr id $ SL01.compress bs
    writeStreamBin (cfgCompressPrintBin cfg) (cfgCompressStreamOut cfg) (runPut sl01)

runDecompress :: MonadIO m => CfgDecompress -> m ()
runDecompress cfg = do
    sl01bs <- readStreamBytes $ cfgDecompressStreamIn cfg
    (sl01, remainingBs) <- liftErr id $ runGet sl01bs
    if remainingBs /= mempty then
        exit "TODO SL01 had extra bytes left over"
    else do
        let bs = SL01.decompress sl01
        writeStreamBin (cfgDecompressPrintBin cfg) (cfgDecompressStreamOut cfg) bs
