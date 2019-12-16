module Main where

import Puzzle
import Puzzles.P1
import Puzzles.P2
import Puzzles.P3
import Puzzles.P4
import Puzzles.P5
import Puzzles.P6
import Puzzles.P7
import Puzzles.P8
import Puzzles.P9
import Puzzles.P10
import Puzzles.P11
import Puzzles.P12
import Puzzles.P13
import Puzzles.P14
import Puzzles.P15

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = [p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
