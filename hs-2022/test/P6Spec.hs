module P6Spec where

import TestPrelude

import qualified Puzzles.P6 as P

pt1 = puzzleExample P.inputParser1 P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser2 P.pt2 "pt 2"

spec_p6 :: Spec
spec_p6 = do
  describe "example 1" $ do
    let input = [r|mjqjpqmgbljsphdztnvjfqwrcgsmlb|]

    pt1 input 7
    pt2 input 19
