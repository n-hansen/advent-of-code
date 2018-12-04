module AoC2018.P1 (p1) where

import AoC2018
import Universum

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.IntSet (member,insert)

p1 :: Puzzle
p1 = Puzzle "1" inputParser (pure pt1) (pure pt2)

type Input = [Int]

inputParser :: Parser Input
inputParser = L.signed space L.decimal `endBy` newline


pt1 :: Input -> Text
pt1 = show . sum

pt2 :: Input -> Text
pt2 = show . findRepeatedFreq

findRepeatedFreq :: [Int] -> Int
findRepeatedFreq = firstDupe mempty . scanl (+) 0 . cycle
  where
    firstDupe :: IntSet -> [Int] -> Int
    firstDupe seen (this:rest) =
      if this `member` seen
      then this
      else firstDupe (insert this seen) rest
