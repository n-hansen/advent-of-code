module P6Spec where

import TestPrelude

import Parse (parseMaybe)

import qualified Puzzles.P6 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p6 :: Spec
spec_p6 = do
  describe "example 1" $ do
    let input = [r|3,4,3,1,2
|]

    specify "parser" $
      parseMaybe P.inputParser input
      `shouldBe`
      Just [0,1,1,2,1,0,0,0,0]

    pt1 input 5934
    pt2 input 26984457539
    pure ()
