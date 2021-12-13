module P9Spec where

import TestPrelude

import qualified Puzzles.P9 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p9 :: Spec
spec_p9 = do
  describe "example 1" $ do
    let input = [r|2199943210
3987894921
9856789892
8767896789
9899965678
|]

    pt1 input 15
    pt2 input 1134
