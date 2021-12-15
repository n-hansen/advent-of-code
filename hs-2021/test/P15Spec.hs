module P15Spec where

import TestPrelude

import qualified Puzzles.P15 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p15 :: Spec
spec_p15 = do
  describe "example 1" $ do
    let input = [r|1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
|]

    pt1 input 40
    pt2 input 315
