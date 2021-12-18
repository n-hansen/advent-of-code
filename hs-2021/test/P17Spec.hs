module P17Spec where

import TestPrelude

import qualified Puzzles.P17 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p17 :: Spec
spec_p17 = do
  describe "example 1" $ do
    let input = [r|target area: x=20..30, y=-10..-5|]

    pt1 input 45
    pt2 input 112
