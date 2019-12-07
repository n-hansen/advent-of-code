module Main where

import Puzzle
import Puzzles.P1
import Puzzles.P2
import Puzzles.P3
import Puzzles.P4
import Puzzles.P5
import Puzzles.P6
import Puzzles.P7

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = [p1,p2,p3,p4,p5,p6,p7]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
