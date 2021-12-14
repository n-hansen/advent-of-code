module P12Spec where

import TestPrelude

import qualified Puzzles.P12 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

spec_p12 :: Spec
spec_p12 = do
  describe "example 1" $ do
    let input = [r|start-A
start-b
A-c
A-b
b-d
A-end
b-end
|]

    pt1 input 10
    pt2 input 36
  describe "example 2" $ do
    let input = [r|fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
|]

    pt1 input 226
    pt2 input 3509
