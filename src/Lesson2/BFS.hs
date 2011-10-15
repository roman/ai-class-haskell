module Lesson2.BFS  
  (
    Node(..) 
  , BasicEdge(..)
  , Graph(..)
  , graphEdges
  , graphNodes
  , enumBFS 
  , consumeTillNode
  , parseGraph
  , main
  ) where

import Control.Monad (mapM_)
import Data.Attoparsec.Enumerator (iterParser)
import Data.Enumerator (run_, ($$))
import Data.Enumerator.Binary (enumFile)

import Lesson2.Enumerator
import Lesson2.Types

import Lesson2.BFS.Enumerator
import Lesson2.BFS.Types
import Lesson2.BFS.Parser 

main :: IO ()
main = do 
  -- Parse the graph from Input.txt
  graph <- run_ $ 
           enumFile "src/Lesson2/Input.txt" $$ iterParser parseGraph
  -- Set the initial State
  let startNode = Node (City "Arad")
  -- Set the goal State
  let goalNode  = Node (City "Bucharest")
  -- Run basic BFS
  result <- run_ $ enumBFS startNode graph $$ consumeTillNode goalNode 
  -- Print visit order
  mapM_ print result

