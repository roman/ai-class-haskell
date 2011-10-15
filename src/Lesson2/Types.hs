{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Lesson2.Types where

import Data.Lens.Common (modL, getL)
import Data.Lens.Template (makeLenses)
import Data.Hashable (Hashable(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord(..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set

-------------------------------------------------------------------------------

class EdgeLike e where
  getEdgeSource :: e a -> Node a
  getEdgeSink   :: e a -> Node a
  getEdgeCost   :: e a -> Maybe Integer

-------------------------------------------------------------------------------

newtype City 
  = City { getCityName :: String }
  deriving (Eq, Ord, Hashable)


data Node a
  = Node {
    nodeValue :: a
  }

data Graph e a
  = Graph {
    _graphNodes :: Set (Node a)
  , _graphEdges :: Set (e a)
  }

makeLenses [''Graph]

-------------------------------------------------------------------------------

instance Show City where
  show = show . getCityName

instance Show a => Show (Node a) where
  show (Node n) = show n

instance Hashable a => Hashable (Node a) where
  hash (Node n) = 0 `hashWithSalt` n

instance Hashable a => Eq (Node a) where
  a == b = hash a == hash b

instance (Hashable a) => Ord (Node a) where
  compare = comparing hash

instance (Ord (e a), Hashable a, EdgeLike e) => Monoid (Graph e a) where
  mempty = emptyGraph
  mappend (Graph n0 e0) (Graph n1 e1) =
      Graph (n0 `mappend` n1)
            (e0 `mappend` e1)

-------------------------------------------------------------------------------

emptyGraph :: Hashable a => Graph e a
emptyGraph = Graph Set.empty Set.empty

appendNode :: (Hashable a, EdgeLike e) 
           => Node a 
           -> Graph e a 
           -> Graph e a
appendNode node = modL graphNodes (Set.insert node)


getNodeNeighbours :: (Hashable a, EdgeLike e) 
                  => Node a 
                  -> Graph e a 
                  -> [(Maybe Integer, Node a)]
getNodeNeighbours n =
    Set.fold getOtherNode [] .
    getL graphEdges
  where
    getOtherNode e acc 
      | getEdgeSource e == n = (getEdgeCost e, getEdgeSink e) : acc
      | otherwise            = acc

