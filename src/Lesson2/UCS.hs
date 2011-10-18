module Lesson2.UCS (
    main
  , Graph(..)
  , UCSEdge(..)
  , Node(..)
  ) where

import Control.Monad (forM_)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Enumerator (run_, ($$), (=$))
import Data.Enumerator.Binary (enumFile)
import Data.Maybe (fromMaybe)

import Navigation.Enumerator
import Lesson2.Enumerator
import Lesson2.Types

import Lesson2.UCS.Enumerator
import Lesson2.UCS.Parser
import Lesson2.UCS.Types

main :: IO () 
main = do
  graph <- run_ $ 
           enumFile "src/Lesson2/Input.txt" $$ iterParser parseGraph
  -- Set the initial State
  let startNode = Node (City "Arad")
  -- Set the goal State
  let goalNode  = Node (City "Bucharest")
  -- Run basic UCS
  result <- run_ $ enumUCS startNode graph $$ removeVisited =$ consumeTillNode goalNode 
  -- Print visit order
  forM_ result $ \event -> do
    putStr $ "[cost: " ++ show (nvCost event) ++ "] "
    putStr $ show $ fromMaybe (nvVal event) (nvParent event)
    putStr " -> "
    putStrLn $ show (nvVal event)

