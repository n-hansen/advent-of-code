{-# LANGUAGE OverloadedLists #-}
import qualified Test.Tasty
import Test.Tasty.Hspec

import Data.List
import qualified Data.Vector as V

import qualified Puzzles.P1 as P1
import qualified Puzzles.P2 as P2

main :: IO ()
main = Test.Tasty.defaultMain =<< testSpec "advent-of-code-2019" spec

spec :: Spec
spec = parallel $ do
  describe "puzzle 1" $ do
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

  describe "puzzle 2" $ do
    describe "part 1" $ do
      let shouldEvaluateTo initial final = P2.runProgram initial == final
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
