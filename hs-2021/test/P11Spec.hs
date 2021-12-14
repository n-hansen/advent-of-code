module P11Spec where

import TestPrelude

import qualified Puzzles.P11 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p11 :: Spec
spec_p11 = do
  describe "example 1" $ do
    let input = [r|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
|]

    pt1 input 1656
    pt2 input 195
