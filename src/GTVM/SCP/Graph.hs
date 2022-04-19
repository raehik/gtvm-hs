{-|
TODO 2022-04-19T16:24:21+0100

Nice! This works. But it creates a very small graph that pauses at the first
map, because they use a different SCP jump command (0x2B). So there's more
complexity. Would be nice to use labelled edges to indicate which jump type.
-}

module GTVM.SCP.Graph where

import GTVM.SCP
import GTVM.Flowchart qualified as Flowchart
import GTVM.Flowchart ( Flowchart )

import Refined.WithRefine ( PredicateStatus(..), unWithRefine )

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled ( Graph )
import Algebra.Graph.Export.Dot qualified as Graph.Dot

import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Maybe ( mapMaybe )
import Data.Set ( Set )
import Data.Set qualified as Set

import System.FilePath ( (</>) )
import Data.ByteString qualified as BS
import GTVM.Common.IO ( badParseYAML )

import GHC.Generics ( Generic )
import Data.String ( IsString )

import Control.Monad.Reader
import Control.Monad.Except

newtype SCPID = SCPID { getSCPID :: Text }
    deriving stock   (Generic, Eq, Show, Ord)
    deriving newtype (IsString)

scpIDAsFilePath :: SCPID -> FilePath
scpIDAsFilePath = Text.unpack . getSCPID

data Node = Node
  { nodeSCPID   :: SCPID
  , nodeChapter :: Text
  } deriving stock (Generic, Eq, Show)

data JumpType
  = RegJump -- ^ jump via 07 command, regular text script jump
  | MapJump -- ^ jump via 2B command, probably in map scripts only
    deriving stock (Generic, Eq, Show, Ord)

findSCPJumps :: SCP Text -> [(SCPID, JumpType)]
findSCPJumps = mapMaybe go
  where
    go = \case
      SCPSeg07SCP scp      -> Just $ (SCPID scp, RegJump)
      SCPSeg2B _ _ _ scp _ -> Just $ (SCPID scp, MapJump)
      _ -> Nothing

-- TODO No. Chapter name retrieval can be done after graph creation.
class (Monad m, MonadError String m) => MonadSCPFolder m where
    readSCP :: SCPID -> m (SCP Text)
    findChapter :: SCPID -> m Text

data GraphEnv = GraphEnv
  { graphEnvFolder :: FilePath
  , graphEnvFlowchart :: Flowchart 'Unenforced Text
  } deriving stock (Generic, Eq, Show)

-- Lazy error lifting.
liftShowEither :: (MonadError String m, Show e) => String -> Either e a -> m a
liftShowEither msg = either (throwError . (\s -> msg<>": "<>s). show) return

instance MonadIO m => MonadSCPFolder (ReaderT GraphEnv (ExceptT String m)) where
    readSCP scpID = do
        scpFolder <- asks graphEnvFolder
        let scpFile = scpIDAsFilePath scpID <> ".scp.yaml"
        scp <- liftIO $ (BS.readFile $ scpFolder </> scpFile) >>= badParseYAML
        return scp

    findChapter scpID = do
        fc <- asks graphEnvFlowchart
        case Flowchart.findEntryViaScript (getSCPID scpID) fc of
          Nothing ->
            throwError $ "couldn't find SCP in flowchart: " <> scpIDAsFilePath scpID
          Just entry -> do
            return $ unWithRefine $ Flowchart.entryName entry

-- | Starting from one SCPID, recursively build a graph of jumps.
--
-- No tail recursion. Written in a way that would be parallel, but we have to do
-- I/O.
buildGraph :: MonadSCPFolder m => SCPID -> m (Graph (Set JumpType) SCPID)
buildGraph = go Set.empty
  where
    go seen scpID
     | Set.member scpID seen = return Graph.empty
     | otherwise = do
        scp <- readSCP scpID
        let scpOuts = findSCPJumps scp
            g = Graph.edges $ map (\(so, jt) -> (Set.singleton jt, scpID, so)) scpOuts
            seen' = Set.insert scpID seen
        gs <- mapM (go seen') $ map fst scpOuts
        return $ Graph.overlays $ g : gs

-- | Starting from one SCPID, tail recursively build a graph of jumps.
buildGraphTail :: forall m. MonadSCPFolder m => SCPID -> m (Graph (Set JumpType) SCPID)
buildGraphTail stmp = go Graph.empty Set.empty [stmp]
  where
    go :: Graph (Set JumpType) SCPID -> Set SCPID -> [SCPID] -> m (Graph (Set JumpType) SCPID)
    go g seen = \case
      []   -> return g
      s:ss ->
        if   Set.member s seen
        then go g seen ss
        else do scp <- readSCP s
                let scpOuts = findSCPJumps scp
                    sg = Graph.edges $ map (\(so, jt) -> (Set.singleton jt, s, so)) scpOuts
                go (Graph.overlay g sg) (Set.insert s seen) (map fst scpOuts <> ss)

buildGraph'
    :: MonadIO m
    => FilePath -> FilePath -> SCPID -> m (Graph (Set JumpType) SCPID)
buildGraph' scpFolder flowchartFilepath scpID = do
    prepFlowchart flowchartFilepath >>= \case
      Left  e  -> error $ "error reading flowchart: "<>e
      Right fc -> do
        let graphEnv = GraphEnv scpFolder fc
        runExceptT (runReaderT (buildGraphTail scpID) graphEnv) >>= \case
          Left  e -> error $ "error: "<>e
          Right g   -> return g

prepFlowchart :: MonadIO m => FilePath -> m (Either String (Flowchart 'Unenforced Text))
prepFlowchart _fp = return $ Right undefined

style :: Graph.Dot.Style SCPID String
style = Graph.Dot.Style
  { Graph.Dot.graphName               = "scp"
  , Graph.Dot.preamble                = mempty
  , Graph.Dot.graphAttributes         = mempty
  , Graph.Dot.defaultVertexAttributes = mempty
  , Graph.Dot.defaultEdgeAttributes   = mempty
  , Graph.Dot.vertexName              = \x   -> Text.unpack $ getSCPID x
  , Graph.Dot.vertexAttributes        = \_x    -> mempty
  , Graph.Dot.edgeAttributes          = \_x _y -> mempty
  , Graph.Dot.attributeQuoting        = Graph.Dot.DoubleQuotes
  }
