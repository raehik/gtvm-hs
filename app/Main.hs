module Main where

import           Config
import           CLI

import qualified GTVM.Assorted.Flowchart as GAFc
import qualified GTVM.Common.Binary.Util as GCBU
import qualified GTVM.Common.Binary      as GCB

import qualified Data.Aeson               as Aeson
import qualified Data.Aeson.Encode.Pretty as DAEP
import qualified Data.ByteString.Lazy     as BL

main :: IO ()
main = parseCLIOpts >>= runCmd

runCmd :: ToolGroup -> IO ()
runCmd = \case
  TGFlowchart cfg -> runCmdFlowchart cfg

runCmdFlowchart :: TGFlowchartCfg -> IO ()
runCmdFlowchart (TGFlowchartCfg dir ty fp) =
    case dir of
      ActionDirectionDecode -> do
        lexed <- GCBU.runParserBinFile GAFc.pFlowchart fp GCB.binCfgSCP
        case lexed of
          Left err      -> putStr err
          Right lexed' -> do
            case ty of
              CfgFlowchartTypeParse -> do
                let parsed = GAFc.fcToAltFc lexed'
                putJSON parsed
              _                     -> putStrLn "unsupported"
      ActionDirectionEncode -> do
        putStrLn "Ignoring flowchart type SRY"
        bytes <- BL.readFile fp
        case Aeson.decode bytes of
          Nothing -> putStrLn "Unable to parse file"
          Just flowchart -> do
            let serialized = GAFc.sFlowchart flowchart GCB.binCfgSCP
            putStrLn "well I did it but it's bytes. not gonna print. TODO only allow writing to file, or require force flag"

putJSON :: Aeson.ToJSON a => a -> IO ()
putJSON a = do
    BL.putStr $ DAEP.encodePretty' prettyCfg a
    putStrLn ""
  where
    prettyCfg = DAEP.defConfig { DAEP.confIndent = DAEP.Spaces 2 }
