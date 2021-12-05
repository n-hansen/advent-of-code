module P5Spec where

import TestPrelude

import qualified Puzzles.P5 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p5 :: Spec
spec_p5 = do
  describe "example 1" $ do
    let input = [r|0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
|]

    pt1 input 5
    pt2 input 12
