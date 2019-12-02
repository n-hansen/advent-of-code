import qualified Test.Tasty
import Test.Tasty.Hspec

import Data.List

import qualified Puzzles.P1 as P1

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
        in all (\(mass, fuelReq) -> P1.fuelRequired mass == fuelReq) examples
    describe "part 2" $ do
      it "computes fuel requirements, recursively" $
        let examples = [ (14,     2)
                       , (1969,   966)
                       , (100756, 50346)
                       ]
        in all (\(mass, fuelReq) -> P1.fuelRequired' mass == fuelReq) examples
