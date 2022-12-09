module Puzzles.P4 where

import AocPrelude
import Parse
import Puzzle

p4 :: Puzzle
p4 = Puzzle "4" inputParser1 inputParser2 pt1 pt2

type Input1 = [(Range,Range)]
type Input2 = Input1

type Range = (Int,Int)

inputParser1 :: Parser Input1
inputParser1 = elfPair `endBy1` newline
  where
    elfPair =
      (,)
      <$> range
      <* string ","
      <*> range
    range =
      (,)
      <$> unsignedInteger
      <* string "-"
      <*> unsignedInteger

inputParser2 :: Parser Input2
inputParser2 = inputParser1

isSubrangeOf (a0,a1) (b0,b1) =
  a0 <= b0 && a1 >= b1

pt1 = Just . length . filter (liftM2 (||) (uncurry isSubrangeOf) (uncurry $ flip isSubrangeOf))

overlaps (a0,a1) (b0,b1) =
  not $ a0 > b1 || b0 > a1

pt2 = Just . length . filter (uncurry overlaps)
