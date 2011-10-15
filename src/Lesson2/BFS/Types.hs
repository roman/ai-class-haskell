{-# LANGUAGE TemplateHaskell #-}
module Lesson2.BFS.Types where

import Data.Lens.Common (getL, modL)
import Data.Lens.Template (makeLenses)
import Data.List (foldl', sort)
import Data.Hashable (Hashable(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------

data BasicGraph a
  = BasicGraph {
    _basicGraphNodes :: Set (BasicNode a)
  , _basicGraphEdges :: Set (BasicEdge a)
  }

data BasicNode a
  = BasicNode {
    basicNodeValue :: a
  }

data BasicEdge a
  = BasicEdge {
    basicEdgeSource :: BasicNode a
  , basicEdgeSink   :: BasicNode a
  }

makeLenses [''BasicGraph]

-------------------------------------------------------------------------------

instance Show a => Show (BasicNode a) where
  show (BasicNode v) = show v

instance Show a => Show (BasicEdge a) where
  show (BasicEdge a b) = "(" ++ show a ++ ", " ++ show b ++ ")"

instance Hashable a => Monoid (BasicGraph a) where
  mempty  = emptyGraph
  mappend (BasicGraph n0 e0) (BasicGraph n1 e1) = 
    BasicGraph (n0 `mappend` n1)
               (e0 `mappend` e1)


instance Hashable a => Hashable (BasicNode a) where
  hash (BasicNode a) = 0 `hashWithSalt` a

instance Hashable a => Hashable (BasicEdge a) where
  hash (BasicEdge a b) = foldl' hashWithSalt 1 [a, b]

instance Hashable a => Eq (BasicNode a) where
  a == b = hash a == hash b

instance (Hashable a) => Ord (BasicNode a) where
  compare = comparing hash

instance Hashable a => Eq (BasicEdge a) where
  a == b = hash a == hash b

instance Hashable a => Ord (BasicEdge a) where
  compare = comparing hash

-------------------------------------------------------------------------------

emptyGraph :: BasicGraph a
emptyGraph = BasicGraph Set.empty Set.empty

appendNode :: Hashable a => BasicNode a -> BasicGraph a -> BasicGraph a
appendNode node = modL basicGraphNodes (Set.insert node)

appendEdge :: Hashable a 
           => BasicNode a 
           -> BasicNode a 
           -> BasicGraph a 
           -> BasicGraph a
appendEdge a b  = modL basicGraphEdges (Set.union otherSet)
  where
    otherSet = Set.fromList [BasicEdge a b, BasicEdge b a]

-------------------------------------------------------------------------------

getNodeNeighbours :: Hashable a => BasicNode a -> BasicGraph a -> [BasicNode a]
getNodeNeighbours n =
    Set.fold getOtherNode [] .
    getL basicGraphEdges
  where
    getOtherNode e acc 
      | basicEdgeSource e == n = basicEdgeSink e : acc
      | otherwise              = acc


