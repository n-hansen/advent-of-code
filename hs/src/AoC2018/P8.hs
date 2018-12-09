{-# LANGUAGE NamedFieldPuns #-}
module AoC2018.P8 (p8) where

import           AoC2018
import           Universum

import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

p8 :: Puzzle
p8 = Puzzle "8" inputParser (pure pt1) mempty

data Tree = Node { children :: [Tree]
                 , metadata :: [Int]
                 } deriving (Show,Eq,Ord)

inputParser :: Parser Tree
inputParser = parseNode
  where
    parseNumber = L.decimal <* space
    parseNode = do
      nChildren <- parseNumber
      nMetadata <- parseNumber
      children <- replicateM nChildren parseNode
      metadata <- replicateM nMetadata parseNumber
      pure $ Node children metadata

pt1 :: Tree -> Text
pt1 = show . sumMetadata
  where
    sumMetadata Node{children,metadata} =
      sum $ metadata <> fmap sumMetadata children
