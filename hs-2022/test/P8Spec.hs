module P8Spec where

import TestPrelude

import qualified Puzzles.P8 as P

pt1 = puzzleExample P.inputParser1 P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser2 P.pt2 "pt 2"

spec_p8 :: Spec
spec_p8 = do
  describe "example 1" $ do
    let input = [r|30373
25512
65332
33549
35390
|]

    pt1 input 21
    pt2 input 8
