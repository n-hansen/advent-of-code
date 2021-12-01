module Template where

import AocPrelude
import Puzzle
{-PUZZLE_IMPORTS-}

import System.Environment (getArgs)

puzzles :: [Puzzle]
puzzles = [{-PUZZLE_FNS-}]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ solvePuzzle
    . filter ((arg ==) . toS . puzzleId)
    $ puzzles
