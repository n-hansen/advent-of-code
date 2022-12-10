module P9Spec where

import TestPrelude

import qualified Puzzles.P9 as P

pt1 = puzzleExample P.inputParser1 P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser2 P.pt2 "pt 2"

spec_p9 :: Spec
spec_p9 = do
  describe "example 1" $ do
    let input = [r|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
|]

    pt1 input 13
    pt2 input 1

  describe "example 2" $ do
    let input = [r|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
|]

    pt2 input 36
