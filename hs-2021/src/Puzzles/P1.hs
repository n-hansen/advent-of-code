module Puzzles.P1 where

import AocPrelude
import Parse
import Puzzle

import Util

p1 :: Puzzle
p1 = Puzzle "1" inputParser pt1 pt2

type Input = [Int]

inputParser :: Parser Input
inputParser = signedInteger `endBy` newline

pt1 xs = Just $
  xs
  & window2
  & filter (uncurry (<))
  & length

pt2 xs = Just $
  xs
  & window 3
  & fmap sum
  & window2
  & filter (uncurry (<))
  & length
