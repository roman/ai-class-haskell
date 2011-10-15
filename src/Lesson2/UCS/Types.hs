module Lesson2.UCS.Types where

import Data.Hashable (Hashable(..))
import Data.Lens.Common (modL)
import Data.List (foldl')
import Data.Ord (Ord(..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set

import Lesson2.Types

-------------------------------------------------------------------------------

type UCSGraph = Graph UCSEdge

data UCSEdge a
  = UCSEdge {
    ucsEdgeSource :: Node a
  , ucsEdgeSink   :: Node a
  , ucsCost       :: Integer
  }

-------------------------------------------------------------------------------

instance Show a => Show (UCSEdge a) where
  show (UCSEdge a b c) =
      "(" ++ show a ++ ", " ++ show b ++ ") -> " ++ show c

instance Hashable a => Hashable (UCSEdge a) where
  hash (UCSEdge a b c) = foldl' hashWithSalt 2 [a, b] + hash c

instance Hashable a => Eq (UCSEdge a) where
  a == b = hash a == hash b

instance Hashable a => Ord (UCSEdge a) where
  compare = comparing hash

instance EdgeLike UCSEdge where
  getEdgeSource = ucsEdgeSource
  getEdgeSink   = ucsEdgeSink
  getEdgeCost   = Just . ucsCost

-------------------------------------------------------------------------------

appendEdge :: Hashable a
           => Node a
           -> Node a
           -> Integer
           -> Graph UCSEdge a
           -> Graph UCSEdge a
appendEdge a b cost = modL graphEdges (Set.union otherSet)
  where
    otherSet = Set.fromList [UCSEdge a b cost, UCSEdge b a cost]

