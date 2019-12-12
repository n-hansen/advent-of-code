{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P11 where

import Parse
import Puzzle

import IntcodeComputer

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

type Input = Tape

inputParser :: Parser Input
inputParser = tapeParser

type Coord = (Int,Int)
data Direction = N | E | S | W
               deriving (Eq,Ord,Enum,Show)

data RobotState = RS { _isWhite :: Set Coord
                     , _hasPainted :: Set Coord
                     , _facing :: Direction
                     , _location :: Coord
                     , _computer :: ComputerState
                     } deriving Show
makeLenses ''RobotState

initialState = RS Set.empty Set.empty N (0,0)

stepForward st =
  case st ^. facing of
    N -> st & location . _2 +~ 1
    S -> st & location . _2 -~ 1
    E -> st & location . _1 +~ 1
    W -> st & location . _1 -~ 1

turn 0 = over facing $ \case
  N -> W
  W -> S
  S -> E
  E -> N

turn 1 = over facing $ \case
  N -> E
  E -> S
  S -> W
  W -> N

paint c st = st
             & hasPainted %~ Set.insert here
             & updateColor
  where
    here = st ^. location
    updateColor = case c of
                    0 -> isWhite %~ Set.delete here
                    1 -> isWhite %~ Set.insert here

mainLoop init = go . init . initProgram
  where
    go st = let input = if Set.member (st ^. location) (st ^. isWhite) then 1 else 0
            in case view computer st & provideInput [input] & runProgram of
                 Halted (Output (c:_)) -> st & paint c
                 Halted _              -> st
                 WaitingForInput (DrainedOutput [color, turnDirection] cpu) ->
                   st
                   & computer .~ cpu
                   & paint color
                   & turn turnDirection
                   & stepForward
                   & go

pt1 = Just . views hasPainted Set.size . mainLoop initialState

initialState' = RS (Set.singleton (0,0)) Set.empty N (0,0)

display st = T.unlines $ do
  let painted = st ^. isWhite . to Set.toList
      xs = fmap fst painted
      ys = fmap snd painted
      top = maximum ys
      bot = minimum ys
      lft = minimum xs
      rgt = maximum xs
      renderCell y x = if views isWhite (Set.member (x,y)) st then "X" else " "
  y <- [top,top-1..bot]
  pure $ foldMap (renderCell y) [lft..rgt]


pt2 = Just . display . mainLoop initialState'



p11 :: Puzzle
p11 = Puzzle "11" inputParser pt1 pt2
