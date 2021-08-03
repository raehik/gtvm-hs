{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}

module GTVM.SCP.Analyse where

import           GTVM.SCP
import           Control.Lens               hiding ((:<))
import           Control.Lens.TH
import           Data.Foldable
import           Data.Functor.Foldable.TH
import           Data.Fix

-- functor product
data (f :*: g) a = (:*:) { left :: f a, right :: g a }
    deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

type AnnoSCPSegment a = (SCPSegment, a)

scpAnnoDef :: [SCPSegment] -> [AnnoSCPSegment ()]
scpAnnoDef = fmap $ \seg -> (seg, ())

{-
annotateSCPSegments :: [AnnoSCPSegment a] -> [AnnoSCPSegment Int]
annotateSCPSegments = fst $ foldl' go ([], 0)
  where
    go :: [AnnoSCPSegment Int] -> AnnoSCPSegment a -> [AnnoSCPSegment Int]
    go a b = a
-}
