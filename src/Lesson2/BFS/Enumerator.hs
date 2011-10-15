module Lesson2.BFS.Enumerator where

import Control.Monad.Trans (MonadIO, liftIO)
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
import Data.Ord (Ord(..), comparing)

import qualified Data.Set as Set

import Lesson2.Types
import Lesson2.BFS.Types

-------------------------------------------------------------------------------

newtype FrontierEntry a
  = FE { fromFE :: (Int, Node a) }
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

enumBFS :: (MonadIO m, Show a, Hashable a)
        => Node a
        -> BFSGraph a
        -> Enumerator (Node a) m b
enumBFS source0 g =
    go Set.empty (Set.singleton $ FE (0, source0))
  where
    go _ _ step@(Yield {}) = returnI step
    go explored0 frontier0 step@(Continue consumer) = Iteratee $
      case Set.minView frontier0 of
        Just (FE (i, source), frontier1) -> do
          let explored = Set.insert source explored0
          let frontier = Set.union frontier1                        .
                         Set.fromList                               .
                         Prelude.map (\(_, n) -> FE (i + 1, n))          .
                         Prelude.filter ((`Set.notMember` explored) . snd)  $
                         getNodeNeighbours source g
          runIteratee $ consumer (Chunks [source]) >>== go explored frontier
        Nothing -> return step

