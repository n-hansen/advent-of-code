module P1Spec where

import TestPrelude

import qualified Puzzles.P1 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p1 :: Spec
spec_p1 = do
  describe "example 1" $ do
    let input = [r|199
200
208
210
200
207
240
269
260
263
|]
    pt1 input 7
    pt2 input 5
