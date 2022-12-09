module P2Spec where

import TestPrelude

import qualified Puzzles.P2 as P

pt1 = puzzleExample P.inputParser1 P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser2 P.pt2 "pt 2"

spec_p2 :: Spec
spec_p2 = do
  describe "example 1" $ do
    let input = [r|A Y
B X
C Z
|]

    pt1 input 15
    pt2 input 12
