module P1Spec where

import TestPrelude

import Parse
import qualified Puzzles.P1 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p1 :: Spec
spec_p1 = do
  describe "example 1" $ do
    let input = [r|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
|]

    specify "parser" $
      parseEither P.inputParser input
      `shouldBe`
      Right [[1000,2000,3000],[4000],[5000,6000],[7000,8000,9000],[10000]]

    pt1 input 24000
    -- pt2 input ???
