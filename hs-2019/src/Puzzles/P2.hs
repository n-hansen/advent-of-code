{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P2 where

import Puzzle

import Parse

import           Data.Vector (Vector)
import qualified Data.Vector as V

data ComputerState = CS { _position :: Int
                        , _tape :: Vector Int
                        } deriving (Eq, Show)
makeLenses ''ComputerState

p2 :: Puzzle
p2 = Puzzle "2" inputParser pt1 pt2

type Input = ComputerState

inputParser :: Parser Input
inputParser = CS 0 . V.fromList <$> (signedInteger `sepBy` "," <* optional newline)

pt1 = Just . getAnswer <=< runProgram . updateTape
  where
    updateTape = (tape . ix 1 .~ 12) . (tape . ix 2 .~ 2)
    getAnswer = preview $ tape . ix 0

pt2 st = Just findAnswer
  where
    findAnswer = headMay [100 * noun + verb | noun <- [0..100], verb <- [0..100], checkValues noun verb st]
    checkValues noun verb = (Just 19690720 ==) . (getAnswer <=< runProgram) . updateTape noun verb
    updateTape noun verb = (tape . ix 1 .~ noun) . (tape . ix 2 .~ verb)
    getAnswer = preview $ tape . ix 0

runProgram :: ComputerState -> Maybe ComputerState
runProgram st = arg 0 >>= eval
  where
    arg n = st ^? tape . ix (st ^. position . to (+ n))
    deref i = st ^? tape . ix i
    advance n = position %~ (+ n)

    eval 1
      | Just val1 <- deref =<< arg 1
      , Just val2 <- deref =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ val1 + val2
        & advance 4
        & runProgram

    eval 2
      | Just val1 <- deref =<< arg 1
      , Just val2 <- deref =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ val1 * val2
        & advance 4
        & runProgram

    eval 99 = pure st
