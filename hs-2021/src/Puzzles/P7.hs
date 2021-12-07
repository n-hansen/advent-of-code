{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P7 where

import AocPrelude
import Parse
import Puzzle
import Util

p7 :: Puzzle
p7 = Puzzle "7" inputParser pt1 pt2

type Input = [Int]

inputParser :: Parser Input
inputParser = (unsignedInteger `sepBy` string ",") <* newline

calculateAlignmentCosts :: [(Int,Int)] -> [Int]
calculateAlignmentCosts =
  uncurry (zipWith (+))
  . (scanCosts &&& reverse . scanCosts . reverse)
  where
    scanCosts ((initPos, initCnt) :< rest) =
      rest
      & scanl (\(currPos,currCnt,currFuel) (nextPos, nextCnt)
               -> ( nextPos
                  , currCnt + nextCnt
                  , currFuel + currCnt * abs (nextPos - currPos)
                  )
              ) (initPos, initCnt, 0)
      & fmap (view _3)

pt1 =
  Just
  . minimum
  . calculateAlignmentCosts
  . sparseHistogram

calculateAlignmentCosts' :: [Int] -> [Int]
calculateAlignmentCosts' =
  uncurry (zipWith (+))
  . (scanCosts &&& reverse . scanCosts . reverse)
  where
    scanCosts (initCnt :< rest) =
      rest
      & scanl (\(currCnt, currUsed, currCost) nextCnt
               -> ( currCnt + nextCnt
                  , currUsed + currCost
                  , currCost + currCnt + nextCnt
                  )
              ) (initCnt, 0, initCnt)
      & fmap (view _2)

pt2 =
  Just
  . minimum
  . calculateAlignmentCosts'
  . fmap snd
  . fullHistogram
