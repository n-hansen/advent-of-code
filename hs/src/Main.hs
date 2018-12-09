module Main where

import           Universum

import           System.Environment (getArgs)

import           AoC2018
import           AoC2018.P1
import           AoC2018.P2
import           AoC2018.P3
import           AoC2018.P4
import           AoC2018.P5
import           AoC2018.P6
import           AoC2018.P7
import           AoC2018.P8

puzzles :: [Puzzle]
puzzles = [p1,p2,p3,p4,p5,p6,p7,p8]

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \arg ->
    mapM_ (solvePuzzle >=> putStrLn)
    . filter ((== arg) . puzzleId)
    $ puzzles
