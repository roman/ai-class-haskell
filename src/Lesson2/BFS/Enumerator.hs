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

import Lesson2.BFS.Types

-------------------------------------------------------------------------------

newtype FrontierEntry a
  = FE { fromFE :: (Int, BasicNode a) }
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

enumBFS :: (MonadIO m, Show a, Hashable a)
        => BasicNode a
        -> BasicGraph a
        -> Enumerator (BasicNode a) m b
enumBFS source0 g =
    go Set.empty (Set.singleton $ FE (0, source0))
  where
    go _ _ step@(Yield {}) = returnI step
    go explored0 frontier0 step@(Continue consumer) = Iteratee $
      case Set.minView frontier0 of
        Just (FE (i, source), frontier1) -> do
          let explored = Set.insert source explored0
          let frontier = Set.union frontier1                       .
                         Set.fromList                              .
                         Prelude.map (\n -> FE (i + 1, n))         .
                         Prelude.filter (`Set.notMember` explored) $ 
                         getNodeNeighbours source g
          runIteratee $ consumer (Chunks [source]) >>== go explored frontier
        Nothing -> return step

-------------------------------------------------------------------------------

consumeTillNode :: (Monad m, Hashable a)
                => BasicNode a 
                -> Iteratee (BasicNode a) m [BasicNode a]
consumeTillNode dest = continue $ go []
  where
    go acc EOF = yield [] EOF
    go acc (Chunks ns) = Iteratee $ do
      let (as, bs) = span (dest /=) ns
      case bs of 
        []     -> return $ Continue $ go $ acc ++ as
        (x:xs) -> return $ Yield (acc ++ as ++ [x]) (Chunks xs)

-------------------------------------------------------------------------------

