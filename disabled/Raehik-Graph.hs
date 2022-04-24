-- | Directed, labelled, dynamic, context-based representation (lots of
--   bookkeeping, but efficient parent access).
--
-- I have a graph transformation that requires efficient read access to parents.
-- It appears this isn't well-handled by the graph representations provided by
-- popular Haskell libraries. I need something like an adjacency matrix, where
-- you can have 0 or 1 directed edge between 2 vertices, and accessing a
-- vertex's parents or children is done by checking either its row or its
-- column.
--
-- I've devised an efficient algorithm that works
-- best with contexts, where we can easily read/write vertex parents.
--
-- hash-graph looks unfinished, but seems to have the representation and
-- fundamentals I want.

module Raehik.Graph where

import Data.HashGraph.Strict qualified as Graph
import Data.HashGraph.Strict ( Gr, (!) )

import Data.Map qualified as Map
import Data.Map ( Map )
import Data.Set qualified as Set
import Data.HashSet qualified as HashSet
import Data.HashSet ( HashSet )
import Data.Hashable ( Hashable )

import GHC.Generics ( Generic )

import Debug.Trace ( trace )

data JumpType
  = RegJump -- ^ jump via 07 command, regular text script jump
  | MapJump -- ^ jump via 2B command, probably in map scripts only
    deriving stock (Generic, Eq, Show, Ord)

-- n ~ SCPID
-- l ~ JumpType
-- Map keys are vertices.
-- Map values do not exist in the graph, and are unique per key.
-- @top@ must be a vertex.
compressGraph :: (Eq n, Ord n, Hashable n, Hashable l, Show n) => n -> Gr l n -> (Gr l n, Map n [(n, l)])
compressGraph top = go [top] Set.empty Map.empty
  where
    go []     _    compr g = (g, compr)
    go (focus:foci) seen compr g
     | Set.member focus seen = go foci seen compr g
     | otherwise =
        let vContext = g ! focus
            vSuccs   = Graph.tails vContext
        in  case HashSet.toList vSuccs of
              [Graph.Tail l s] -> -- focus has 1 successor: possible candidate
                let sContext = g ! s
                 in case HashSet.toList (Graph.heads sContext) of
                      [_p] -> -- focus child has 1 parent (p ~ focus): candidate
                        let sSuccs = Graph.tails sContext
                            g'  = Graph.delNode s g -- delete node fully
                            g'' = graphInternalInsEdges focus sSuccs g' -- "re"-connect focus's children to focus
                            foci' = focus:foci -- reschedule ourselves
                        in  go (focus:foci) seen compr g''
                      _    -> -- focus child does not have 1 parent: not a candidate
                        go (s:foci) seen' compr g
              ss  -> -- focus does not have 1 successor: not a candidate
                go (map (\(Graph.Tail l n) -> n) ss<>foci) seen' compr g
          where seen' = Set.insert focus seen

-- Insert edges into a hash-graph graph using internals.
graphInternalInsEdges :: (Hashable n, Hashable l) => n -> HashSet (Graph.Tail l n) -> Gr l n -> Gr l n
graphInternalInsEdges head' tails gInit = foldr go gInit tails
  where
    go (Graph.Tail l n) g = Graph.insEdge (Graph.Edge head' l n) g

graphInternalTailNodes :: HashSet (Graph.Tail l n) -> [n]
graphInternalTailNodes = map (\(Graph.Tail l n) -> n) . HashSet.toList

ex1 :: Gr String String
ex1 = Graph.mkGraph edges nodes
  where
    nodes = ["1"]
    edges = []

ex2 :: Gr () String
ex2 = Graph.mkGraph edges nodes
  where
    nodes = ["1", "2", "3", "4", "5"]
    edges =
      [ Graph.Edge "1" () "2"
      , Graph.Edge "1" () "3"
      , Graph.Edge "2" () "4"
      , Graph.Edge "4" () "5"
      , Graph.Edge "3" () "5"
      ]

ex3 :: Gr () String
ex3 = Graph.mkGraph edges nodes
  where
    nodes = ["1", "2", "3", "4", "5", "6"]
    edges =
      [ Graph.Edge "1" () "2"
      , Graph.Edge "1" () "3"
      , Graph.Edge "2" () "4"
      , Graph.Edge "4" () "5"
      , Graph.Edge "5" () "6"
      , Graph.Edge "3" () "6"
      ]

-- TODO tmp
graphPretty :: (Show n, Show l) => Gr l n -> String
graphPretty = Graph.pretty
