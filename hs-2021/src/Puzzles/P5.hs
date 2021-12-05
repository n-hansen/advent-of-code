module Puzzles.P5 where

import AocPrelude
import Parse
import Puzzle

type Coord = (Int,Int)

data Line = L { _start :: Coord
              , _end :: Coord
              } deriving (Eq,Show)

p5 :: Puzzle
p5 = Puzzle "5" inputParser pt1 pt2

type Input = [Line]

inputParser :: Parser Input
inputParser = line `endBy1` newline
  where
    line = L <$> coord <* string " -> " <*> coord
    coord = (,) <$> signedInteger <* string "," <*> signedInteger

pointsOnLine :: Bool -> Line -> [Coord]
pointsOnLine allowDiagonal (L (x1,y1) (x2,y2))
  | x1 == x2 = do
      y <- smartRange y1 y2
      pure (x1,y)
  | y1 == y2 = do
      x <- smartRange x1 x2
      pure (x,y1)
  | not allowDiagonal = []
  | otherwise = zip (smartRange x1 x2) (smartRange y1 y2)
  where
    smartRange a b = if a <= b then [a..b] else [a,a-1..b]

pt1 = Just
  . length
  . filter ((1 <) . length)
  . group
  . sort
  . concatMap (pointsOnLine False)

pt2 = Just
  . length
  . filter ((1 <) . length)
  . group
  . sort
  . concatMap (pointsOnLine True)
