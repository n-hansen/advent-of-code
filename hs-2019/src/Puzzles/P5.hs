module Puzzles.P5 where

import Parse
import Puzzle

import IntcodeComputer


p5 :: Puzzle
p5 = Puzzle "5" inputParser pt1 pt2

type Input = Tape

inputParser = tapeParser

pt1 input = Just $ do
  Halted (Output o) <- initProgram input
                       & provideInput [1]
                       & runProgram
                       & pure
  lastMay o

pt2 input = Just $ do --Nothing :: Maybe (Maybe Int)
  Halted (Output o) <- initProgram input
                       & provideInput [5]
                       & runProgram
                       & pure
  lastMay o
