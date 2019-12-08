module Puzzles.P8 where

import Parse
import Puzzle
import Util

p8 :: Puzzle
p8 = Puzzle "8" inputParser pt1 pt2

type Input = [Char]

inputParser :: Parser Input
inputParser = many digitChar

reshape width height = fmap makeLayer . chunks (width * height)
  where
    makeLayer = chunks width
    chunks n = unfoldr $ \xs -> if null xs then Nothing else Just (splitAt n xs)

pt1 = Just
      . getAnswer
      . minimumBy (compare `on` countThing '0')
      . reshape 25 6
  where
    countThing x = length . filter (== x) . mconcat
    getAnswer xs = countThing '1' xs * countThing '2' xs

flattenImage = fmap (fmap flattenLayers . transpose) . transpose
  where
    flattenLayers ('2':xs) = flattenLayers xs
    flattenLayers (x:_)  = x

render = foldMap renderRow
  where
    renderRow r = foldMap renderCell r <> "\n"
    renderCell '0' = " "
    renderCell '1' = "X"

pt2 = Just
      . render
      . flattenImage
      . reshape 25 6

