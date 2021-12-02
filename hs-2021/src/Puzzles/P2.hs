{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P2 where

import AocPrelude hiding (Down)
import Parse
import Puzzle

import Optics.TH

data Position = Pos { _horiz :: Int
                    , _depth :: Int
                    , _aim :: Int
                    } deriving (Eq,Show,Generic)

makeLenses ''Position

p2 :: Puzzle
p2 = Puzzle "2" inputParser pt1 pt2

data Movement
  = Forward Int
  | Down Int
  | Up Int
  deriving (Eq,Show,Generic)

move (Forward x) = horiz %~ (+ x)
move (Down x) =  depth %~ (+ x)
move (Up x) = depth %~ subtract x

move' (Forward x) p = p
                      & horiz %~ (+ x)
                      & depth %~ (+ p ^. aim * x)
move' (Down x) p = p & aim %~ (+ x)
move' (Up x) p = p & aim %~ subtract x

type Input = [Movement]

inputParser :: Parser Input
inputParser = movement `endBy` newline
  where
    movement = ($) <$> direction <*> unsignedInteger
    direction = choice [ string "up" >> space >> pure Up
                       , string "down" >> space >> pure Down
                       , string "forward" >> space >> pure Forward
                       ]

pt1 mv = Just $
  let Pos horiz depth _ = foldl (&) (Pos 0 0 0) . fmap move $ mv
  in horiz * depth

pt2 mv = Just $
  let Pos horiz depth _ = foldl (&) (Pos 0 0 0) . fmap move' $ mv
  in horiz * depth

