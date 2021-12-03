module Puzzles.P3 where

import AocPrelude
import Parse
import Puzzle
import Data.List

p3 :: Puzzle
p3 = Puzzle "3" inputParser pt1 pt2

type Input = [[Bool]]

inputParser :: Parser Input
inputParser = many bit `endBy` newline
  where
    bit = (string "0" >> pure False) <|> (string "1" >> pure True)

toDecimal :: [Bool] -> Int
toDecimal =
  fst .
  foldr (\bit (acc, exp) ->
          ( if bit then acc + exp else acc
          , exp * 2)
          ) (0,1)

pt1 input = Just result
  where
    inputLength = length input
    popCounts =
      input
      & transpose
      & fmap (length . filter identity)
    gamma =
      popCounts
      & fmap (> inputLength `div` 2)
      & toDecimal
    epsilon =
      popCounts
      & fmap (<= inputLength `div` 2)
      & toDecimal
    result = gamma * epsilon

pt2 input = Just result
  where
    partitionByBit n = partition (\bs -> unsafeIndex bs n)
    o2rating = findRating True 0 input
    co2rating = findRating False 0 input
    result = o2rating * co2rating

    findRating _ _ [x] = toDecimal x
    findRating pickLongest ix xs =
      findRating pickLongest (ix+1) $
      let (ones, zeros) = partitionByBit ix xs
      in if (length ones >= length zeros) == pickLongest
         then ones
         else zeros
