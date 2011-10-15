{-# LANGUAGE TemplateHaskell #-}
module Lesson2.UCS.Types where

import Data.Hashable (Hashable(..))
import Data.Lens.Common (getL, modL)
import Data.Lens.Template (makeLenses)
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------

data UCSGraph a 
  = UCSGraph {
    _ucsGraphNodes :: Set (UCSNode a)
  , _ucsGraphEdges :: Set (UCSEdge a)
  }

data UCSNode a
  = UCSNode {
    ucsNodeValue :: a
  }

data UCSEdge a
  = UCSEdge {
    ucsEdgeSource :: UCSNode a
  , ucsEdgeSink   :: UCSNode a
  , ucsCost       :: Integer
  }

makeLenses [''UCSGraph]

-------------------------------------------------------------------------------

instance Show a => Show (UCSNode a) where
  show (UCSNode n) = show n

instance Show a => Show (UCSEdge a) where
  show (UCSEdge a b c) = 
      "(" ++ show a ++ ", " ++ show b ++ ") -> " ++ show c

instance Hashable a => Monoid (UCSGraph a) where
  mempty  = emptyGraph
  mappend (UCSGraph n0 e0) (UCSGraph n1 e1) = 
    UCSGraph (n0 `mappend` n1)
               (e0 `mappend` e1)

instance Hashable a => Hashable (UCSNode a) where
  hash (UCSNode a) = 1 `hashWithSalt` a

instance Hashable a => Hashable (UCSEdge a) where
  hash (UCSEdge a b c) = foldl' hashWithSalt 2 [a, b] + hash c

instance Hashable a => Eq (UCSNode a) where
  a == b = hash a == hash b

instance (Hashable a) => Ord (UCSNode a) where
  compare = comparing hash

instance Hashable a => Eq (UCSEdge a) where
  a == b = hash a == hash b

instance Hashable a => Ord (UCSEdge a) where
  compare = comparing hash

-------------------------------------------------------------------------------

emptyGraph :: UCSGraph a
emptyGraph = UCSGraph Set.empty Set.empty

appendNode :: Hashable a => UCSNode a -> UCSGraph a -> UCSGraph a
appendNode node = modL ucsGraphNodes (Set.insert node)

appendEdge :: Hashable a 
           => UCSNode a 
           -> UCSNode a 
           -> Integer
           -> UCSGraph a 
           -> UCSGraph a
appendEdge a b cost = modL ucsGraphEdges (Set.union otherSet)
  where
    otherSet = Set.fromList [UCSEdge a b cost, UCSEdge b a cost]


-------------------------------------------------------------------------------

getNodeNeighbours :: Hashable a => UCSNode a -> UCSGraph a -> [(Integer, UCSNode a)]
getNodeNeighbours n =
    Set.fold getOtherNode [] .
    getL ucsGraphEdges
  where
    getOtherNode e acc 
      | ucsEdgeSource e == n = (ucsCost e, ucsEdgeSink e) : acc
      | otherwise            = acc

