module Lesson2.BFS.Types where

import Data.Lens.Common (modL)
import Data.List (foldl')
import Data.Hashable (Hashable(..))
import Data.Ord (Ord(..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set


import Lesson2.Types

-------------------------------------------------------------------------------

type BFSGraph = Graph BasicEdge

data BasicEdge a
  = BasicEdge {
    basicEdgeSource :: Node a
  , basicEdgeSink   :: Node a
  }

-------------------------------------------------------------------------------

instance Show a => Show (BasicEdge a) where
  show (BasicEdge a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance Hashable a => Hashable (BasicEdge a) where
  hash (BasicEdge a b) = foldl' hashWithSalt 1 [a, b]

instance Hashable a => Eq (BasicEdge a) where
  a == b = hash a == hash b

instance Hashable a => Ord (BasicEdge a) where
  compare = comparing hash

instance EdgeLike BasicEdge where
  getEdgeSource = basicEdgeSource
  getEdgeSink   = basicEdgeSink
  getEdgeCost   = const Nothing

-------------------------------------------------------------------------------

appendEdge :: Hashable a
           => Node a
           -> Node a
           -> Graph BasicEdge a
           -> Graph BasicEdge a
appendEdge a b = modL graphEdges (Set.union otherSet)
  where
    otherSet = Set.fromList [BasicEdge a b, BasicEdge b a]

