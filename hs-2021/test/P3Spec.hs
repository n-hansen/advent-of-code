module P3Spec where

import TestPrelude

import qualified Puzzles.P3 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p3 :: Spec
spec_p3 = do
  describe "example 1" $ do
    let input = [r|00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
|]

    pt1 input 198
    pt2 input 230
