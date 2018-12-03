module Main where

import Universum

import System.Environment (getArgs)

import qualified AoC2018.P1.Pt1 as P1a (run)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("1a":input:_) -> readFile input >>= P1a.run
    _ -> print "not sure what to do"
