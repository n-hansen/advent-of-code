module Puzzles.P1 where

import AocPrelude
import Parse
import Puzzle

p1 :: Puzzle
p1 = Puzzle "1" inputParser pt1 pt2

type Input = [[Int]]

inputParser :: Parser Input
inputParser = (unsignedInteger `sepEndBy` newline) `sepBy` newline

pt1 = Just . maximum . fmap sum

pt2 = Just . sum . take 3 . sortBy (flip compare) . fmap sum
