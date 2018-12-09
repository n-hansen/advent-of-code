{-# LANGUAGE NamedFieldPuns #-}
module AoC2018.P8 (p8) where

import           AoC2018
import           Universum                  hiding (mapMaybe)

import           Data.Vector                (Vector, fromList, mapMaybe, (!?))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

p8 :: Puzzle
p8 = Puzzle "8" inputParser (pure pt1) (pure pt2)

data Tree = Node { children :: Vector Tree
                 , metadata :: Vector Int
                 } deriving (Show)

inputParser :: Parser Tree
inputParser = parseNode
  where
    parseNumber = L.decimal <* space
    parseNode = do
      nChildren <- parseNumber
      nMetadata <- parseNumber
      children <- fromList <$> replicateM nChildren parseNode
      metadata <- fromList <$> replicateM nMetadata parseNumber
      pure $ Node children metadata

pt1 :: Tree -> Text
pt1 = show . sumMetadata
  where
    sumMetadata Node{children,metadata} =
      sum $ metadata <> fmap sumMetadata children

pt2 :: Tree -> Text
pt2 = show . computeCheck
  where
    computeCheck Node{children,metadata}
      | null children = sum metadata
      | otherwise     = sum
                        . mapMaybe (\md -> computeCheck <$> children !? (md-1))
                        $ metadata
