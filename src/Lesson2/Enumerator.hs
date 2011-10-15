module Lesson2.Enumerator where

import Data.Hashable (Hashable)
import Data.Enumerator (
    Stream(..)
  , Iteratee(..)
  , Step(..)
  , continue
  , yield
  )

import Lesson2.Types

consumeTillNode :: (Monad m, Hashable a)
                => Node a
                -> Iteratee (Node a) m [Node a]
consumeTillNode dest = continue $ go []
  where
    go acc EOF = yield [] EOF
    go acc (Chunks []) = continue $ go acc
    go acc (Chunks ns) = Iteratee $ do
      let (as, bs) = span (dest /=) ns
      case bs of
        []     -> return $ Continue $ go $ acc ++ as
        (x:xs) -> return $ Yield (acc ++ as ++ [x]) (Chunks xs)

