{-| Tools to traverse a folder of SCPs and generate various graphs.

TODO I have a POC, I can make a graph of the full game (disregarding jumps in
the eboot). But it's not useful as-is. Things to do:

  * Slim by converting to basic blocks. Each block stores a non-empty list of
    SCPs. Calculate a basic block by selecting a yet-processed node and
    consuming succeeding nodes until you reach one that either has multiple
    parents (don't consume, schedule), or has multiple children (consume,
    schedule children).
    * This is kinda more like inlining? Basic blocks are easy because you do can
      do a marking phase, then a cleanup phase. We can't do that here because
      the only operation is jump, we just want to cleanup certain jumps. So I
      think it's slow, due to checking parents all the time.
    * I think you make an entirely new graph. Starting from the same node, build
      up 1) a basic block graph, and 2) a basic block info map. The graph is
      just edges between SCPIDs, the map stores the basic block info. You have
      to do the same seen thing (because I dunno recursion-schemes for graphs).
  * Group by flowchart group.
  * Gather more data when we process an SCP. For example, count character lines!
    Then you can get an idea of who the scene is about.

I think I could do a @Graph -> (Graph, Map Node [Node])@ where the map provides
extra data about the nodes (their "contained" successive SCPs, empty allowed).
@Graph@ can be done by editing the original graph. Each individual compression
deletes 1 edge (disconnect parent & child), deletes 1 vertex (child), and
updates between 1-n edges (relocates child children to parent). This algorithm
would run best on a graph representation that enables efficient access to node
contexts (= children & parents).

hash-graph (not on Hackage: https://github.com/patrickdoc/hash-graph ) is
probably best if I want a context-based algorithm, it's based on them. More so
than fgl, which also may work? But it seems like more work, and it's a shitty
interface. However, it's got tons of todos related to safety, for basic things.
So I'm not sure. Maybe I have to write my own representation -- but it would be
inspired by hash-graph, so I should start by trying that first.
-}

module GTVM.SCP.Graph where

import GTVM.SCP
import GTVM.Flowchart qualified as Flowchart
import GTVM.Flowchart ( Flowchart )

import Refined.WithRefine ( PredicateStatus(..), unWithRefine )

import Algebra.Graph.Labelled qualified as Graph
import Algebra.Graph.Labelled ( Graph )
import Algebra.Graph.Export.Dot qualified as Graph.Dot

import Data.HashGraph.Strict qualified as HashGraph
import Data.HashGraph.Strict ( Gr )

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
    deriving IsString via Text

scpIDAsFilePath :: SCPID -> FilePath
scpIDAsFilePath = Text.unpack . getSCPID

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

data GraphEnv = GraphEnv
  { graphEnvFolder :: FilePath
  , graphEnvFlowchart :: Flowchart 'Unenforced Text
  } deriving stock (Generic, Eq, Show)

-- Lazy error lifting.
liftShowEither :: (MonadError String m, Show e) => String -> Either e a -> m a
liftShowEither msg = either (throwError . (\s -> msg<>": "<>s). show) return

-- | Starting from one SCPID, tail recursively build a graph of jumps.
buildGraph
    :: forall m. MonadIO m
    => FilePath -> SCPID -> m (Graph (Set JumpType) SCPID)
buildGraph scpFolder stmp = go Graph.empty Set.empty [stmp]
  where
    go :: Graph (Set JumpType) SCPID -> Set SCPID -> [SCPID] -> m (Graph (Set JumpType) SCPID)
    go g seen = \case
      []   -> return g
      s:ss ->
        if   Set.member s seen
        then go g seen ss
        else do scp <- retrieveSCPID scpFolder s
                let scpOuts = findSCPJumps scp
                    sg = Graph.edges $ map (\(so, jt) -> (Set.singleton jt, s, so)) scpOuts
                go (Graph.overlay g sg) (Set.insert s seen) (map fst scpOuts <> ss)

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

{-
findChapter
    :: (MonadReader GraphEnv m, MonadExcept String m)
    => SCPID -> m Text
findChapter scpID = do
    fc <- asks graphEnvFlowchart
    case Flowchart.findEntryViaScript (getSCPID scpID) fc of
      Nothing ->
        throwError $ "couldn't find SCP in flowchart: " <> scpIDAsFilePath scpID
      Just entry -> do
        return $ unWithRefine $ Flowchart.entryName entry
-}

-- | Starting from one SCPID, tail recursively build a graph of jumps.
buildGraphHG
    :: forall m. MonadIO m
    => FilePath -> SCPID -> m (Graph (Set JumpType) SCPID)
buildGraphHG scpFolder stmp = go HashGraph.empty Set.empty [stmp]
  where
    go :: Graph (Set JumpType) SCPID -> Set SCPID -> [SCPID] -> m (Graph (Set JumpType) SCPID)
    go g seen = \case
      []   -> return g
      s:ss ->
        if   Set.member s seen
        then go g seen ss
        else do scp <- retrieveSCPID scpFolder s
                let scpOuts = findSCPJumps scp
                    edges   = map (\(so, jt) -> HashGraph.Edge s jt so) scpOuts
                    g'      = foldr HashGraph.insEdge g edges
                go g' (Set.insert s seen) (map fst scpOuts <> ss)

retrieveSCPID :: MonadIO m => FilePath -> SCPID -> m (SCP Text)
retrieveSCPID scpFolder scpID = do
    liftIO $ (BS.readFile $ scpFolder </> scpFile) >>= badParseYAML
  where scpFile = scpIDAsFilePath scpID <> ".scp.yaml"
