module Lesson2.UCS.Enumerator where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Lens.Common (getL)
import Data.Enumerator (
   Stream(..)
  , Step(..)
  , Iteratee(..)
  , Enumerator
  , (>>==)
  , returnI
  , continue
  , yield
  )
import Data.Hashable (Hashable(..))
import Data.Maybe (fromJust)

import qualified Data.Set as Set

import Navigation.Enumerator
import Lesson2.Types
import Lesson2.UCS.Types

-------------------------------------------------------------------------------

enumUCS :: (MonadIO m, Show a, Hashable a)
        => Node a
        -> UCSGraph a
        -> Enumerator (NavEvent (Node a)) m b
enumUCS zero g =
    enumNavigation findCost
                   (return . (`getNodeNeighbours` g))
                   zero
  where
    findCost parent child = 
      return .
      head .
      Set.fold (processEdge parent child) [] $ 
      getL graphEdges g
    processEdge parent child e acc 
      | getEdgeSource e == parent && 
        getEdgeSink e == child = fromJust (getEdgeCost e) : acc
      | otherwise = acc

