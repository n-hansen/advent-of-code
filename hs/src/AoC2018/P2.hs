module AoC2018.P2 where

import Universum hiding (elems)

import Data.HashMap.Strict (insertWith,elems)

type Input = [[Char]]

parseInput :: Text -> Input
parseInput = fmap toString . lines

runPt1 :: Text -> IO ()
runPt1 = print . computeChecksum . fmap countLetterReps . parseInput
  where
    countLetterReps = foldl' (\repMap ltr -> insertWith (+) ltr 1 repMap) mempty
    computeChecksum repMaps = countReps 2 repMaps * countReps 3 repMaps
    countReps n = length . filter (not . null . filter (== n) . elems)
