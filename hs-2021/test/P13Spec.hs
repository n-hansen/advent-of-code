module P13Spec where

import TestPrelude

import qualified Puzzles.P13 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p13 :: Spec
spec_p13 = do
  describe "example 1" $ do
    let input = [r|6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
|]

    pt1 input 17
    -- pt2 input ???
