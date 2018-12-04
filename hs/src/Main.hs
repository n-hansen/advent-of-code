module Main where

import Universum

import System.Environment (getArgs)

import AoC2018
import AoC2018.P1
import AoC2018.P2
import AoC2018.P3

puzzles :: [Puzzle]
puzzles = [p1,p2,p3]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ (solvePuzzle >=> putStrLn)
    . filter ((== arg) . puzzleId)
    $ puzzles
