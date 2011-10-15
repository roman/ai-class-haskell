module Lesson2.UCS (
    main
  , UCSGraph(..)
  , UCSEdge(..)
  , UCSNode(..)
  ) where

import Data.Attoparsec.Enumerator (iterParser)
import Data.Enumerator (run_, ($$))
import Data.Enumerator.Binary (enumFile)

import Lesson2.Types
import Lesson2.UCS.Enumerator
import Lesson2.UCS.Parser
import Lesson2.UCS.Types

main :: IO () 
main = do
  graph <- run_ $ 
           enumFile "src/Lesson2/Input.txt" $$ iterParser parseGraph
  -- Set the initial State
  let startNode = UCSNode (City "Arad")
  -- Set the goal State
  let goalNode  = UCSNode (City "Bucharest")
  -- Run basic UCS
  result <- run_ $ enumUCS startNode graph $$ consumeTillNode goalNode 
  -- Print visit order
  mapM_ print result

