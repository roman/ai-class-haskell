module Exploration.Types where

import Data.Set (Set)

-------------------------------------------------------------------------------

data NavEvent a
  = NavEvent {
    nvVal            :: a
  , nvParent         :: Maybe a
  , nvCost           :: Integer
  , nvAlreadyVisited :: Bool
  , nvVisited        :: Set a
  , nvFrontier       :: Set (Integer, a, Maybe a)
  }
  deriving (Show, Ord, Eq)

