module Main where

import AocPrelude
import Puzzle
import Puzzles.P2
import Puzzles.P6
import Puzzles.P3
import Puzzles.P4
import Puzzles.P1
import Puzzles.P5

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = [p2,p6,p3,p4,p1,p5]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
