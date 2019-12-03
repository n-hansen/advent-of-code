{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P2 where

import Puzzle

import Control.Lens
import Control.Lens.TH

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

pt1 = Just . getAnswer . runProgram . updateTape
  where
    updateTape = (tape . ix 1 .~ 12) . (tape . ix 2 .~ 2)
    getAnswer = preview $ tape . ix 0

pt2 st = Just findAnswer
  where
    findAnswer = headMay [100 * noun + verb | noun <- [0..100], verb <- [0..100], checkValues noun verb st]
    checkValues noun verb = (Just 19690720 ==) . getAnswer . runProgram . updateTape noun verb
    updateTape noun verb = (tape . ix 1 .~ noun) . (tape . ix 2 .~ verb)
    getAnswer = preview $ tape . ix 0

runProgram st
  | Just 1    <- st ^? tape . ix pos
  , Just arg1 <- st ^? tape . ix (pos + 1)
  , Just arg2 <- st ^? tape . ix (pos + 2)
  , Just arg3 <- st ^? tape . ix (pos + 3)
  , Just val1 <- st ^? tape . ix arg1
  , Just val2 <- st ^? tape . ix arg2
  , Just _    <- st ^? tape . ix arg3
  = st
    & tape . ix arg3 .~ val1 + val2
    & advance 4
    & runProgram
  | Just 2    <- st ^? tape . ix pos
  , Just arg1 <- st ^? tape . ix (pos + 1)
  , Just arg2 <- st ^? tape . ix (pos + 2)
  , Just arg3 <- st ^? tape . ix (pos + 3)
  , Just val1 <- st ^? tape . ix arg1
  , Just val2 <- st ^? tape . ix arg2
  , Just _    <- st ^? tape . ix arg3
  = st
    & tape . ix arg3 .~ val1 * val2
    & advance 4
    & runProgram
  | Just 99 <- st ^? tape . ix pos
  = st
  where
    pos = st ^. position
    advance x = position %~ (+ x)
