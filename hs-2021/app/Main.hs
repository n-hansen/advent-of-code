module Main where

import AocPrelude
import Puzzle
import Puzzles.P1

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = [p1]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
