module Main where

import AocPrelude
import Puzzle
import Puzzles.P8
import Puzzles.P13
import Puzzles.P12
import Puzzles.P9
import Puzzles.P16
import Puzzles.P2
import Puzzles.P6
import Puzzles.P7
import Puzzles.P3
import Puzzles.P4
import Puzzles.P1
import Puzzles.P5
import Puzzles.P11
import Puzzles.P15
import Puzzles.P14
import Puzzles.P10

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = [p8,p13,p12,p9,p16,p2,p6,p7,p3,p4,p1,p5,p11,p15,p14,p10]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
