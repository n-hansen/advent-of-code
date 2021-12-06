module Puzzles.P6 where

import AocPrelude
import Parse
import Puzzle

import qualified Data.Sequence as Seq

p6 :: Puzzle
p6 = Puzzle "6" inputParser pt1 pt2

type Input = Seq Int

inputParser :: Parser Input
inputParser = toFreqList <$> (unsignedInteger `sepBy1` string ",") <* space
  where
    toFreqList =
      Seq.fromList
      . fmap (subtract 1 . length)
      . group
      . sort
      . ([0..8] <>)

step (multiplying :< rest) =
  rest |> multiplying
  & ix 6 %~ (+ multiplying)

pt1 =
  fmap sum
  . head
  . drop 80
  . iterate step

pt2 =
  fmap sum
  . head
  . drop 256
  . iterate step
