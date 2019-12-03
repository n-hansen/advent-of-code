module Puzzles.P1 where

import Parse
import Puzzle

p1 :: Puzzle
p1 = Puzzle "1" inputParser pt1 pt2

type Input = [Int]

inputParser :: Parser Input
inputParser = signedInteger `endBy` newline

pt1 = Just . sum . fmap fuelRequired

pt2 = Just . sum . fmap fuelRequired'

fuelRequired mass = div mass 3 - 2

fuelRequired' = sum . takeWhile (> 0) . unsafeTail . iterate (max 0 . fuelRequired)
