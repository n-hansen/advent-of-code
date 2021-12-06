module Puzzles.P6 where

import AocPrelude
import Parse
import Puzzle

import qualified Data.Sequence as Seq

p6 :: Puzzle
p6 = Puzzle "6" inputParser pt1 pt2

type Input = [Int]

inputParser :: Parser Input
inputParser = (unsignedInteger `sepBy1` string ",") <* space

pt1 =
  fmap sum
  . head -- [a] -> Maybe a
  . drop 80
  . iterate (\case f :< fs -> fs |> f & ix 6 %~ (+ f)) -- pattern synonym and operators from `optics-core`
  . Seq.fromList -- faster but unnecessary
  . fmap (subtract 1 . length)
  . group
  . sort
  . ([0..8] <>)

pt2 =
  fmap sum
  . head
  . drop 256
  . iterate (\case f :< fs -> fs |> f & ix 6 %~ (+ f))
  . Seq.fromList
  . fmap (subtract 1 . length)
  . group
  . sort
  . ([0..8] <>)
