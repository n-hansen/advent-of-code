module Main where

import Universum

import System.Environment (getArgs)

import qualified AoC2018.P1 as P1 (runPt1,runPt2)
import qualified AoC2018.P3 as P3 (runPt1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("1a":input:_) -> readFile input >>= P1.runPt1
    ("1b":input:_) -> readFile input >>= P1.runPt2
    ("3a":input:_) -> readFile input >>= P3.runPt1
    _ -> print "not sure what to do"
