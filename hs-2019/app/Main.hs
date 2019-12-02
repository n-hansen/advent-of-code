module Main where

import Puzzle

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = []

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
