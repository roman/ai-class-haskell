module Lesson2.Enumerator where

import Data.Hashable (Hashable)
import Data.Enumerator (
    Stream(..)
  , Iteratee(..)
  , Step(..)
  , continue
  , yield
  )

import Exploration.Types
import Lesson2.Types

consumeTillNode :: (Monad m, Hashable a)
                => Node a
                -> Iteratee (NavEvent (Node a)) m [NavEvent (Node a)]
consumeTillNode dest = continue $ go []
  where
    go acc EOF = yield [] EOF
    go acc (Chunks []) = continue $ go acc
    go acc (Chunks ns) = Iteratee $ do
      let (as, bs) = span ((dest /=) . nvVal) ns
      case bs of
        []     -> return $ Continue $ go $ acc ++ as
        (x:xs) -> return $ Yield (acc ++ as ++ [x]) (Chunks xs)

