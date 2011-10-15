module Lesson2.BFS.Parser where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Attoparsec.Char8
import Data.ByteString.UTF8 (toString)
import Data.Monoid (mconcat)

import Lesson2.Types
import Lesson2.BFS.Types

tillComma = takeTill (== ',')
tillEOL = takeTill (== '\n')
comma = char ','
eol = char '\n'

parseCity =
    entry <$> (skipSpace *>
               tillComma <*
               comma)
  where
    entry = BasicNode . City . toString

parseEntry :: Parser (BasicGraph City)
parseEntry = do
    a <- parseCity
    b <- parseCity
    tillEOL
    eol
    return $ createGraph a b
  where
    createGraph a b =
      appendEdge a b  .
      appendNode a    $
      appendNode b emptyGraph

parseGraph :: Parser (BasicGraph City)
parseGraph = mconcat <$> many1 parseEntry

