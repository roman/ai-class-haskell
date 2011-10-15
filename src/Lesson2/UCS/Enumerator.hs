module Lesson2.UCS.Enumerator where

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

import Lesson2.UCS.Types

-------------------------------------------------------------------------------

newtype FrontierEntry a
  = FE { fromFE :: (Integer, UCSNode a) }
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------

enumUCS :: (MonadIO m, Show a, Hashable a)
        => UCSNode a
        -> UCSGraph a
        -> Enumerator (UCSNode a) m b
enumUCS source0 g =
    go Set.empty (Set.singleton $ FE (0, source0))
  where
    go _ _ step@(Yield {}) = returnI step
    go explored0 frontier0 step@(Continue consumer) = Iteratee $
      case Set.minView frontier0 of
        Just (FE (i, source), frontier1) ->
          if source `Set.member` explored0
            then runIteratee $ consumer (Chunks []) >>== go explored0 frontier1
            else do
              let explored = Set.insert source explored0
              let frontier = Set.union frontier1                        .
                             Set.fromList                               .
                             Prelude.map (\(c, n) -> FE (i + c, n))     .
                             Prelude.filter ((`Set.notMember` explored) .
                                             snd)                       $
                             getNodeNeighbours source g
              runIteratee $ consumer (Chunks [source]) >>== go explored frontier
        Nothing -> return step

-------------------------------------------------------------------------------

consumeTillNode :: (Monad m, Hashable a)
                => UCSNode a
                -> Iteratee (UCSNode a) m [UCSNode a]
consumeTillNode dest = continue $ go []
  where
    go acc EOF = yield [] EOF
    go acc (Chunks []) = continue $ go acc
    go acc (Chunks ns) = Iteratee $ do
      let (as, bs) = span (dest /=) ns
      case bs of
        []     -> return $ Continue $ go $ acc ++ as
        (x:xs) -> return $ Yield (acc ++ as ++ [x]) (Chunks xs)

-------------------------------------------------------------------------------


