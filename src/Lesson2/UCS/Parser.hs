module Lesson2.UCS.Parser where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Attoparsec.Char8
import Data.ByteString.UTF8 (toString)
import Data.Monoid (mconcat)

import Lesson2.Types
import Lesson2.UCS.Types

tillComma = takeTill (== ',')
tillEOL = takeTill (== '\n')
comma = char ','
eol = char '\n'

parseCity =
    entry <$> (skipSpace *>
               tillComma <*
               comma)
  where
    entry = UCSNode . City . toString

parseCost = 
    skipSpace *> decimal <* tillEOL <* eol

parseEntry :: Parser (UCSGraph City)
parseEntry = do
    a <- parseCity
    b <- parseCity
    c <- parseCost
    return $ createGraph a b c
  where
    createGraph a b c =
      appendEdge a b c .
      appendNode a     $
      appendNode b emptyGraph

parseGraph :: Parser (UCSGraph City)
parseGraph = mconcat <$> many1 parseEntry

