module Puzzles.P8 where

import Parse
import Puzzle
import Util

import qualified Data.Text as T
import Data.List.Extra

p8 :: Puzzle
p8 = Puzzle "8" inputParser pt1 pt2

type Input = [Char]

inputParser :: Parser Input
inputParser = many digitChar

reshape width height = chunksOf height . chunksOf width

pt1 = Just
      . getAnswer
      . minimumOn (countOf '0')
      . reshape 25 6
  where
    countOf x = length . filter (== x) . mconcat
    getAnswer = (*) <$> countOf '1' <*> countOf '2'

flattenImage = fmap (fmap flattenLayers . transpose) . transpose
  where
    flattenLayers = unsafeHead . dropWhile (== '2')

render = T.unlines . fmap (foldMap renderCell)
  where
    renderCell '0' = " "
    renderCell '1' = "X"

pt2 = Just
      . render
      . flattenImage
      . reshape 25 6

