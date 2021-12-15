module TestPrelude
  ( module X
  , puzzleExample
  , puzzleInput
  ) where

import AocPrelude as X hiding (Selector)
import Test.Tasty as X
import Test.Tasty.HUnit as X
import Test.Tasty.Hspec as X hiding (after,after_)

import System.IO.Unsafe (unsafePerformIO)
import Parse
import Puzzle (readPuzzleInput)

puzzleExample parser solver title input expectation =
  specify title $
  (parseMaybe parser input >>= solver)
  `shouldBe`
  Just expectation

puzzleInput :: Text -> Text
puzzleInput = unsafePerformIO . readPuzzleInput
