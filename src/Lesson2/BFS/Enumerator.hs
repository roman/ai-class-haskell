module Lesson2.BFS.Enumerator where

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Ord (Ord(..), comparing)

import qualified Data.Set as Set

--------------------

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

--------------------

import Exploration.Enumerator
import Lesson2.Types
import Lesson2.BFS.Types

-------------------------------------------------------------------------------

enumBFS :: (MonadIO m, Hashable a)
        => Node a
        -> BFSGraph a
        -> Enumerator (NavEvent (Node a)) m b
enumBFS source0 g = 
    enumNavigation (const $ return 1)
                   (return . (`getNodeNeighbours` g))
                   source0

