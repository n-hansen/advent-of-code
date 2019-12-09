module Puzzles.P9 where

import Parse
import Puzzle

import IntcodeComputer

p9 :: Puzzle
p9 = Puzzle "9" inputParser pt1 pt2

type Input = Tape

inputParser :: Parser Input
inputParser = tapeParser

pt1 input = Just $ do
  Halted (Output o) <- initProgram input
                       & provideInput [1]
                       & runProgram
                       & Just
  pure o

pt2 input = Just $ do
  Halted (Output o) <- initProgram input
                       & provideInput [2]
                       & runProgram
                       & Just
  pure o
