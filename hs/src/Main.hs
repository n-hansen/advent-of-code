module Main where

import Universum

import System.Environment (getArgs)

import qualified AoC2018.P1 as P1 (runPt1,runPt2)
import qualified AoC2018.P2 as P2 (runPt1,runPt2)
import qualified AoC2018.P3 as P3 (runPt1,runPt2)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("1a":input:_) -> readFile input >>= P1.runPt1
    ("1b":input:_) -> readFile input >>= P1.runPt2
    ("2a":input:_) -> readFile input >>= P2.runPt1
    ("2b":input:_) -> readFile input >>= P2.runPt2
    ("3a":input:_) -> readFile input >>= P3.runPt1
    ("3b":input:_) -> readFile input >>= P3.runPt2
    _ -> print "not sure what to do"
