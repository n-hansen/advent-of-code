module Main where

import Puzzle
import Puzzles.P1
import Puzzles.P2
import Puzzles.P3

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = [p1,p2,p3]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
