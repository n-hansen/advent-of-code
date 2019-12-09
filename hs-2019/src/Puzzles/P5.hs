module Puzzles.P5 where

import Parse
import Puzzle

import IntcodeComputer


p5 :: Puzzle
p5 = Puzzle "5" inputParser pt1 pt2

type Input = Tape

inputParser = tapeParser

pt1 input = Just $ lastMay =<< runUntilHalt input [1]

pt2 input = Just $ lastMay =<< runUntilHalt input [5]
