module P2Spec where

import TestPrelude

import qualified Puzzles.P2 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p2 :: Spec
spec_p2 = describe "day 2" $ do
  describe "example 1" $ do
    let input = [r|forward 5
down 5
forward 8
up 3
down 8
forward 2
|]

    pt1 input 150
    pt2 input 900
