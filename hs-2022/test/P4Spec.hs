module P4Spec where

import TestPrelude

import qualified Puzzles.P4 as P

pt1 = puzzleExample P.inputParser1 P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser2 P.pt2 "pt 2"

spec_p4 :: Spec
spec_p4 = do
  describe "example 1" $ do
    let input = [r|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
|]

    pt1 input 2
    pt2 input 4
