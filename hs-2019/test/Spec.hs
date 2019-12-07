{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Test.Tasty
import Test.Tasty.Hspec

import Data.List
import qualified Data.Vector as V
import Text.RawString.QQ

import Parse
import Util

import qualified Puzzles.P1 as P1
import qualified Puzzles.P2 as P2
import qualified Puzzles.P3 as P3
import qualified Puzzles.P4 as P4
import qualified Puzzles.P6 as P6

main :: IO ()
main = Test.Tasty.defaultMain =<< testSpec "advent-of-code-2019" spec

spec :: Spec
spec = parallel $ do
  utilSpec
  puzzle1
  puzzle2
  puzzle3
  puzzle4
  puzzle5
  puzzle6


utilSpec :: Spec
utilSpec = describe "utils" $ do
  specify "window" $
      window 3 [1..5]
      `shouldBe`
      [ [1,2,3]
      , [2,3,4]
      , [3,4,5]
      ]

  specify "window2" $
    window2 [1..4]
    `shouldBe`
    [ (1,2)
    , (2,3)
    , (3,4)
    ]

  specify "window3" $
    window3 [1..5]
    `shouldBe`
    [ (1,2,3)
    , (2,3,4)
    , (3,4,5)
    ]


puzzle1 :: Spec
puzzle1 = describe "puzzle 1" $ do
  describe "part 1" $ do
    it "computes fuel requirements" $
      let examples = [ (12,     2)
                     , (14,     2)
                     , (1969,   654)
                     , (100756, 33583)
                     ]
      in all @[] (\(mass, fuelReq) -> P1.fuelRequired mass == fuelReq) examples

  describe "part 2" $ do
    it "computes fuel requirements, recursively" $
      let examples = [ (14,     2)
                     , (1969,   966)
                     , (100756, 50346)
                     ]
      in all @[] (\(mass, fuelReq) -> P1.fuelRequired' mass == fuelReq) examples


puzzle2 :: Spec
puzzle2 = describe "puzzle 2" $ do
  describe "part 1" $ do
    let shouldEvaluateTo initial final = P2.runProgram initial == Just final
    it "can handle add opcode" $
      P2.CS 0 [1,0,0,0,99]
      `shouldEvaluateTo`
      P2.CS 4 [2,0,0,0,99]
    it "can handle multiply opcode" $
      P2.CS 0 [2,3,0,3,99]
      `shouldEvaluateTo`
      P2.CS 4 [2,3,0,6,99]
    it "can handle multiply opcodes 2" $
      P2.CS 0 [2,4,4,5,99,0]
      `shouldEvaluateTo`
      P2.CS 4 [2,4,4,5,99,9801]
    it "loops until halt" $
      P2.CS 0 [1,1,1,4,99,5,6,0,99]
      `shouldEvaluateTo`
      P2.CS 8 [30,1,1,4,2,5,6,0,99]


puzzle3 :: Spec
puzzle3 = describe "puzzle 3" $ do
  describe "part 1" $ do
    it "enumerates points on a path" $
      P3.enumeratePath [ P3.Directive P3.R 8
                       , P3.Directive P3.U 5
                       , P3.Directive P3.L 5
                       , P3.Directive P3.D 3
                       ]
      `shouldBe`
      [(x,0) | x <- [0..8]]
      <> [(8,y) | y <- [1..5]]
      <> [(x,5) | x <- [7,6..3]]
      <> [(3,y) | y <- [4,3..2]]

    it "finds intersections" $
      let p1 = P3.enumeratePath [ P3.Directive P3.R 8
                                , P3.Directive P3.U 5
                                , P3.Directive P3.L 5
                                , P3.Directive P3.D 3
                                ]
          p2 = P3.enumeratePath [ P3.Directive P3.U 7
                                , P3.Directive P3.R 6
                                , P3.Directive P3.D 4
                                , P3.Directive P3.L 4
                                ]
      in P3.intersections p1 p2
         `shouldMatchList`
         [(3,3), (6,5)]

    describe "provided examples" $ do
      let runExample i1 i2 = parseMaybe P3.inputParser (i1 <> "\n" <> i2) >>= P3.pt1
      specify "example 1" $
        runExample "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
        `shouldBe`
        Just 159

      specify "example 2" $
        runExample "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        `shouldBe`
        Just 135

  describe "part 2" $ do
    describe "provided examples" $ do
      let runExample i1 i2 = parseMaybe P3.inputParser (i1 <> "\n" <> i2) >>= P3.pt2
      specify "example 1" $
        runExample "R8,U5,L5,D3" "U7,R6,D4,L4"
        `shouldBe`
        Just 30

      specify "example 2" $
        runExample "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
        `shouldBe`
        Just 610

      specify "example 3" $
        runExample "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
        `shouldBe`
        Just 410


puzzle4 :: Spec
puzzle4 = describe "puzzle 4" $ do
  specify "part 1" $
    fmap @[] P4.pt1Criteria [111111,223450,123789]
    `shouldBe`
    [True, False, False]

  specify "part 2" $
    fmap @[] P4.pt2Criteria [112233,123444,111122]
    `shouldBe`
    [True,False,True]


puzzle5 :: Spec
puzzle5 = pure () -- TODO


puzzle6 :: Spec
puzzle6 = describe "puzzle 6" $ do
  describe "part 1" $ do
    specify "example" $
      let input = [r|COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
|]
          processed = parseMaybe P6.inputParser input >>= P6.pt1
      in processed `shouldBe` Just 42
