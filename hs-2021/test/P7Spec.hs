module P7Spec where

import TestPrelude

import qualified Puzzles.P7 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p7 :: Spec
spec_p7 = do
  describe "example 1" $ do
    let input = [r|16,1,2,0,4,2,7,1,2,14
|]

    pt1 input 37
    pt2 input 168
