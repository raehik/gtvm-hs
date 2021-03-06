module Main (main) where

import           Config
import qualified CLI as CLI
import           CLI ( ToolGroup(..) )

import qualified GTVM.Assorted.SL01         as GAS
import qualified GTVM.Assorted.Flowchart    as GAFc
import qualified GTVM.Assorted.Pak          as GAP
import qualified GTVM.Common.Binary.Parse   as GCBP
import qualified GTVM.Common.Binary         as GCB
import           GTVM.SCP
import qualified GTVM.SCP.Parse             as GSP
import qualified GTVM.SCP.Serialize         as GSS
import qualified GTVM.SCP.SCPX              as GSX
--import qualified CSV                      as CSV

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Encode.Pretty   as DAEP
import qualified Data.Yaml                  as Yaml
import qualified Data.Yaml.Pretty           as YamlPretty
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString            as BS
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Text                  ( Text )
import qualified Data.List                  as List
import qualified System.Exit                as Exit

import           Control.Monad.IO.Class

import qualified System.FilePath.Find       as Filemanip
import qualified Tool.SCP.Replace

main :: IO ()
main = CLI.parseOpts >>= runCmd

runCmd :: ToolGroup -> IO ()
runCmd = \case
  TGSCP cfg cS2 -> runCmdSCP cS2 cfg
  TGSCPX cS2 -> runCmdSCPX cS2
  TGSL01 cfg cS2 -> runCmdSL01 cS2 cfg
  TGFlowchart cfg cS2 parseType -> runCmdFlowchart cS2 parseType cfg
  TGPak cfg cPrintStdout -> runCmdPak cPrintStdout cfg
  TGCSVPatch cS2 -> runCmdCSV cS2
  TGSCPReplace cfg -> Tool.SCP.Replace.run cfg

runCmdCSV :: MonadIO m => (CStream, CStream) -> m ()
runCmdCSV = error "unimplemented"
{-
runCmdCSV (cSFrom, cSTo) = do
    bs <- rReadStream' cSFrom
    case CSV.csvDecode bs of
      Left err -> liftIO $ print err
      Right csv -> do
        let mps = CSV.csvToTextReplace <$> csv
            mps' = BPP.MultiPatches { BPP.mpsBaseOffset = Just 0, BPP.mpsPatches = mps }
        rWriteStreamBin True cSTo (encodeYamlPretty [mps'])
-}

runCmdSCP :: MonadIO m => (CStream, CStream) -> CJSON -> m ()
runCmdSCP (cSFrom, cSTo) = \case
  CJSONDe cPrettify -> do
    scpBs <- rParseStream (GCBP.parseBin GSP.pSCP GCB.binCfgSCP) cSFrom
    case scpBsToText scpBs of
      Left err -> do
        liftIO $ putStrLn $ "error: Unicode exception while decoding UTF-8 bytestring: " <> show err
        liftIO $ Exit.exitWith (Exit.ExitFailure 4)
      Right scpText ->
        let scpTextJsonBs = encodeYamlPretty scpText
         in rWriteStreamBin True cSTo scpTextJsonBs
  CJSONEn cPrintStdout -> do
    bs <- rReadStream cSFrom
    scp <- rForceParseYAML @(SCP Text) bs
    let scpBs = GSS.sSCP (scpTextToBs scp) GCB.binCfgSCP
    rWriteStreamBin cPrintStdout cSTo scpBs

runCmdSCPX :: MonadIO m => (CStream, CStream) -> m ()
runCmdSCPX (cSFrom, cSTo) = do
    bs       <- rReadStream cSFrom
    scpxText <- rForceParseYAML @(GSX.SCPX Text) bs
    let scpText = GSX.evalSCPX scpxText
        scpTextJsonBs = encodeYamlPretty scpText
    rWriteStreamBin True cSTo scpTextJsonBs

runCmdSL01 :: MonadIO m => (CStream, CStream) -> CBin -> m ()
runCmdSL01 (cSFrom, cSTo) (CBin cDir cPrintStdout) = readBytes cDir >>= writeBytes
  where
    readBytes = \case
      CDirectionFromOrig -> rParseStream fromOrig cSFrom
      CDirectionToOrig   -> do
        bytes <- rReadStream cSFrom
        return $ GAS.sSL01 (GAS.compress bytes) GCB.binCfgSCP
    writeBytes = rWriteStreamBin cPrintStdout cSTo
    fromOrig fp bs = do
        sl01 <- GCBP.parseBin GAS.pSL01 GCB.binCfgSCP fp bs
        return $ GAS.decompress sl01

runCmdFlowchart :: (MonadIO m) => (CStream, CStream) -> CParseType -> CJSON -> m ()
runCmdFlowchart (cSFrom, cSTo) parseType = \case
  CJSONDe cPrettify -> do
    bs <- rParseStream (fromOrig cPrettify) cSFrom
    rWriteStreamBin True cSTo bs
  CJSONEn cPrintStdout -> do
    bs <- rReadStream cSFrom
    fc <- rGetFc bs parseType
    let fcBytes = GAFc.sFlowchart fc GCB.binCfgSCP
    rWriteStreamBin cPrintStdout cSTo fcBytes
  where
    fromOrig cPrettify fp bs = do
        fc <- GCBP.parseBin GAFc.pFlowchart GCB.binCfgSCP fp bs
        case parseType of
          CParseTypeFull    -> return $ encodeJson cPrettify (GAFc.fcToAltFc fc)
          CParseTypePartial -> return $ encodeJson cPrettify fc
    rGetFc bs = \case
      CParseTypeFull    -> GAFc.altFcToFc <$> rForceParseJSON bs
      CParseTypePartial ->                    rForceParseJSON bs

runCmdPak :: (MonadIO m) => Bool -> CPak -> m ()
runCmdPak cPrintStdout = \case
  CPakUnpack (cS1, cSN) -> do
    pak <- rParseStream parseAndExtract cS1
    case cSN of
      CStreamsArchive fp -> do
        liftIO $ putStrLn $ "error: unimplemented: write pak to archive: " <> fp
        liftIO $ Exit.exitWith (Exit.ExitFailure 3)
      CStreamsFolder  fp -> rWritePakFolder pak fp
  CPakPack (cS1, cSN) unk -> do
    case cSN of
      CStreamsArchive fp -> do
        liftIO $ putStrLn $ "error: unimplemented: write pak from archive: " <> fp
        liftIO $ Exit.exitWith (Exit.ExitFailure 3)
      CStreamsFolder  fp -> do
        files <- liftIO $ getDirContentsWithFilenameRecursive fp
        let pak = GAP.Pak unk files
            pakBytes = GAP.sPak pak GCB.binCfgSCP
        rWriteStreamBin cPrintStdout cS1 pakBytes
  where
    parseAndExtract :: FilePath -> BS.ByteString -> Either String GAP.Pak
    parseAndExtract fp bs = do
        pakHeader <- GCBP.parseBin GAP.pPakHeader GCB.binCfgSCP fp bs
        return $ pakExtract bs pakHeader

-- Only gets stuff with content (files).
getDirContentsWithFilenameRecursive :: FilePath -> IO [(Text, BS.ByteString)]
getDirContentsWithFilenameRecursive fp = do
    fileList <-
        Filemanip.find
            (pure True)
            (Filemanip.fileType Filemanip.==? Filemanip.RegularFile)
            fp
    mapM f fileList
  where
    f fp' = do
        bs <- BS.readFile fp'
        let Just fp'' = List.stripPrefix (fp <> "/") fp'
        return (Text.pack fp'', bs)

pakExtract :: BS.ByteString -> GAP.PakHeader -> GAP.Pak
pakExtract bs (GAP.PakHeader unk ft) = GAP.Pak unk (pakExtractFile bs <$> ft)

pakExtractFile :: BS.ByteString -> GAP.PakHeaderFTE -> (Text, BS.ByteString)
pakExtractFile bs (GAP.PakHeaderFTE offset len filenameBS) =
    let fileBs   = bsExtract (fromIntegral offset) (fromIntegral len) bs
        filename = Text.decodeUtf8 filenameBS
     in (filename, fileBs)

-- | Extract a indexed substring from a bytestring. Runtime error if can't.
bsExtract :: Int -> Int -> BS.ByteString -> BS.ByteString
bsExtract offset len bs =
    let bs' = (BS.take len . BS.drop offset) bs
     in if   BS.length bs' /= len
        then error "ya bytes fucked"
        else bs'

--------------------------------------------------------------------------------

rWritePakFolder :: MonadIO m => GAP.Pak -> FilePath -> m ()
rWritePakFolder (GAP.Pak _ files) fp =
    let files' = map (\(a, b) -> (Text.unpack a, b)) files
     in liftIO $ rWriteFilesToFolder fp files'

rWriteFilesToFolder :: MonadIO m => FilePath -> [(FilePath, BS.ByteString)] -> m ()
rWriteFilesToFolder folder = mapM_ f
  where
    f (filename, bytes) = do
        let fp = folder <> "/" <> filename
        liftIO $ putStrLn $ "writing file: " <> fp
        liftIO $ BS.writeFile fp bytes
