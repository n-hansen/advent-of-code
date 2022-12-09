module P5Spec where

import TestPrelude
import Parse

import qualified Puzzles.P5 as P

pt1 = puzzleExample P.inputParser1 P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser2 P.pt2 "pt 2"

spec_p5 :: Spec
spec_p5 = do
  describe "example 1" $ do
    let input = [r|    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|]

    specify "parser" $ do
      fst <$> parseEither P.inputParser1 input
      `shouldBe`
      Right [(1,"NZ"), (2,"DCM"), (3,"P")]

    pt1 input "CMZ"
    pt2 input "MCD"
