module P14Spec where

import TestPrelude

import qualified Puzzles.P14 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p14 :: Spec
spec_p14 = do
  describe "example 1" $ do
    let input = [r|NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
|]

    pt1 input 1588
    pt2 input 2188189693529
