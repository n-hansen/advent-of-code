{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
import qualified Test.Tasty
import Test.Tasty.Hspec

import qualified Data.Text as T
import Text.RawString.QQ

import System.IO.Unsafe (unsafePerformIO)

import Parse
import Util

import qualified IntcodeComputer as IC

import Puzzle (readPuzzleInput)
import qualified Puzzles.P1 as P1
import qualified Puzzles.P2 as P2
import qualified Puzzles.P3 as P3
import qualified Puzzles.P4 as P4
import qualified Puzzles.P5 as P5
import qualified Puzzles.P6 as P6
import qualified Puzzles.P7 as P7
import qualified Puzzles.P8 as P8
import qualified Puzzles.P9 as P9
import qualified Puzzles.P10 as P10
import qualified Puzzles.P11 as P11
import qualified Puzzles.P12 as P12


main :: IO ()
main = Test.Tasty.defaultMain =<< testSpec "advent-of-code-2019" spec

spec :: Spec
spec = parallel $ do
  util
  intcodeComputer
  puzzle1
  puzzle2
  puzzle3
  puzzle4
  puzzle5
  puzzle6
  puzzle7
  puzzle8
  puzzle9
  puzzle10
  puzzle11
  puzzle12


util :: Spec
util = describe "utils" $ do
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


intcodeComputer :: Spec
intcodeComputer = describe "intcode computer" $ do
  let assertFinalTape init input expect =
        case IC.initProgram init
             & IC.provideInput input
             & IC.runProgram
        of
          IC.Halted (IC.Tape t) -> (take (length expect) t) `shouldBe` expect
          r -> expectationFailure $ show r
      assertOutput init input expect =
        case IC.initProgram init
             & IC.provideInput input
             & IC.runProgram
        of
          IC.Halted (IC.Output o) -> o `shouldBe` expect
  it "can handle add opcode" $
    assertFinalTape
    [1,0,0,0,99] []
    [2,0,0,0,99]
  it "can handle multiply opcode" $
    assertFinalTape
    [2,3,0,3,99] []
    [2,3,0,6,99]
  it "can handle multiply opcodes 2" $
    assertFinalTape
    [2,4,4,5,99,0] []
    [2,4,4,5,99,9801]
  it "loops until halt" $
    assertFinalTape
    [1,1,1,4,99,5,6,0,99] []
    [30,1,1,4,2,5,6,0,99]
  it "handles relative mode" $
    assertFinalTape
    [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] []
    [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
  it "handles big numbers" $
    assertOutput
    [1102,34915192,34915192,7,4,7,99,0] []
    [1219070632396864]


puzzleExample parser solver eg input expectation =
  specify ("example " <> show eg) $
  (parseMaybe parser input >>= solver)
  `shouldBe`
  Just expectation

puzzleInput :: Text -> Text
puzzleInput = unsafePerformIO . readPuzzleInput

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
  let input = puzzleInput "2"
  puzzleExample P2.inputParser P2.pt1 1 input $ 3224742
  puzzleExample P2.inputParser P2.pt2 2 input $ [7960]

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
      let example eg i1 i2 = puzzleExample P3.inputParser P3.pt1 eg (i1 <> "\n" <> i2)
      example 1
        "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" 159
      example 2
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" 135

  describe "part 2" $ do
    describe "provided examples" $ do
      let example eg i1 i2 = puzzleExample P3.inputParser P3.pt2 eg (i1 <> "\n" <> i2)
      example 1
        "R8,U5,L5,D3" "U7,R6,D4,L4" 30
      example 2
        "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83" 610
      example 3
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" 410


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
puzzle5 = describe "puzzle 5" $ do
  let input = puzzleInput "5"
  puzzleExample P5.inputParser P5.pt1 1 input $ Just 13285749
  puzzleExample P5.inputParser P5.pt2 2 input $ Just 5000972


puzzle6 :: Spec
puzzle6 = describe "puzzle 6" $ do
  describe "part 1" $ do
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
    puzzleExample P6.inputParser P6.pt1 1 input 42

  describe "part 2" $ do
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
K)YOU
I)SAN
|]
    puzzleExample P6.inputParser P6.pt2 1 input 4

puzzle7 :: Spec
puzzle7 = describe "puzzle 7" $ do
  describe "part 1" $ do
    let example = puzzleExample P7.inputParser P7.pt1
    example 1
      "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" 43210
    example 2
      "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" 54321
    example 3
      "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" 65210

  describe "part 2" $ do
    let example = puzzleExample P7.inputParser P7.pt2
    example 1
      "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" (Just 139629729)
    example 2
      "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
      (Just 18216)

puzzle8 :: Spec
puzzle8 = describe "puzzle 8" $ do
  describe "part 1" $ do
    it "reshapes" $
      P8.reshape 3 2 "123456789012"
      `shouldBe`
      [["123",
        "456"],
       ["789",
        "012"]]

  describe "part 2" $ do
    it "flattens" $
      (P8.flattenImage . P8.reshape 2 2 $ "0222112222120000")
      `shouldBe`
      ["01",
       "10"]

puzzle9 :: Spec
puzzle9 = describe "puzzle 9" $ do
  let input = puzzleInput "9"
  puzzleExample P9.inputParser P9.pt1 1 input 3100786347
  puzzleExample P9.inputParser P9.pt2 2 input 87023

puzzle10 :: Spec
puzzle10 = describe "puzzle 10" $ do
  puzzleExample P10.inputParser P10.pt1 1
    [r|.#..#
.....
#####
....#
...##|]
    8

  puzzleExample P10.inputParser P10.pt2 2
    [r|.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##|]
    (8,2)

puzzle11 :: Spec
puzzle11 = describe "puzzle 11" $ do
  let input = puzzleInput "11"
  puzzleExample P11.inputParser P11.pt1 1 input 1967
  puzzleExample P11.inputParser P11.pt2 2 input $
    T.unlines [ [r|X  X XXX  X  X XXXX  XX  XXXX XXX  X  X|]
              , [r|X X  X  X X  X X    X  X    X X  X X X |]
              , [r|XX   XXX  X  X XXX  X      X  XXX  XX  |]
              , [r|X X  X  X X  X X    X XX  X   X  X X X |]
              , [r|X X  X  X X  X X    X  X X    X  X X X |]
              , [r|X  X XXX   XX  XXXX  XXX XXXX XXX  X  X|]
              ]

puzzle12 :: Spec
puzzle12 = describe "puzzle 12" $ do
  let example = [r|<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>
|]
      Just exampleInput = parseMaybe P12.inputParser example
  describe "part 1" $ do
    specify "single step" $
      P12.step exampleInput
      `shouldMatchList`
      [ P12.P (2, -1,  1) ( 3, -1, -1)
      , P12.P (3, -7, -4) ( 1,  3,  3)
      , P12.P (1, -7,  5) (-3,  1, -3)
      , P12.P (2,  2,  0) (-1, -3,  1)
      ]
    specify "2 steps" $ do
      (unsafeHead . drop 2 . iterate P12.step $ exampleInput)
      `shouldMatchList`
      [ P12.P (5, -3, -1) ( 3, -2, -2)
      , P12.P (1, -2,  2) (-2,  5,  6)
      , P12.P (1, -4, -1) ( 0,  3, -6)
      , P12.P (1, -4,  2) (-1, -6,  2)
      ]

    specify "energy calculation after 10 steps" $ do
      (P12.energy . unsafeHead . drop 10 . iterate P12.step $ exampleInput)
      `shouldBe`
      179

  describe "part 2" $ do
    puzzleExample P12.inputParser P12.pt2 1
      example 2772
