module Puzzles.P9 where

import Parse
import Puzzle

import IntcodeComputer

p9 :: Puzzle
p9 = Puzzle "9" inputParser pt1 pt2

type Input = Tape

inputParser :: Parser Input
inputParser = tapeParser

pt1 input = Just $ headMay =<< runUntilHalt input [1]

pt2 input = Just $ headMay =<< runUntilHalt input [2]
