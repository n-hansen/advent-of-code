{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P17 where

import AocPrelude
import Parse
import Puzzle

import Control.Monad.Loops
import Optics.TH
import Optics.State.Operators

data TargetArea = TA { _xBounds :: (Int,Int)
                     , _yBounds :: (Int,Int)
                     } deriving (Show)

data SimulationState = St { _position :: (Int,Int)
                          , _peak :: Int
                          , _velocity :: (Int,Int)
                          } deriving (Show)

makeLenses ''TargetArea
makeLenses ''SimulationState

p17 :: Puzzle
p17 = Puzzle "17" inputParser pt1 pt2

type Input = TargetArea

inputParser :: Parser Input
inputParser = do
  string "target area: x="
  xmin <- signedInteger
  string ".."
  xmax <- signedInteger
  string ", y="
  ymin <- signedInteger
  string ".."
  ymax <- signedInteger
  optional newline
  pure $ TA (xmin,xmax) (ymin,ymax)

_x = _1
_y = _2

runSimulation :: TargetArea -> (Int,Int) -> Maybe Int
runSimulation target@(TA{_xBounds=(xmin,xmax),_yBounds=(ymin,ymax)}) = loop . St (0,0) 0
  where
    loop :: SimulationState -> Maybe Int
    loop st@St{_position=(x,y),_peak,_velocity=(vx,vy)} = do
      if | xmin <= x && x <= xmax && ymin <= y && y <= ymax -> Just _peak
         | y<ymin -> Nothing
         | x>xmax -> Nothing
         | otherwise -> st
                        & position % _x %~ (+ vx)
                        & position % _y %~ (+ vy)
                        & velocity % _x %~ (if | vx > 0 -> subtract 1
                                               | vx < 0 -> (+ 1)
                                               | otherwise -> identity
                                           )
                        & velocity % _y %~ subtract 1
                        & peak %~ max (y+vy)
                        & loop


pt1 target = Just $ maximum $ do
  vx <- [1..(target ^. xBounds % _2)]
  vy <- [0..1000]
  maybeToList $ runSimulation target (vx,vy)

pt2 target = Just $ length $ do
  vx <- [1..1000]
  vy <- [(target ^. yBounds % _1)..1000]
  () <$ (maybeToList $ runSimulation target (vx,vy))
