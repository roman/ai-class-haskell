{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lesson2.Types where

import Data.Hashable (Hashable(..))

newtype City 
  = City { getCityName :: String }
  deriving (Eq, Ord, Hashable)

instance Show City where
  show = show . getCityName

