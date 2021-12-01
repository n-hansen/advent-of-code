module Puzzles.P1 where

import AocPrelude
import Parse
import Puzzle

p1 :: Puzzle
p1 = Puzzle "1" inputParser pt1 pt2

type Input = Text

inputParser :: Parser Input
inputParser = pure ""

pt1 _ = Nothing :: Maybe ()

pt2 _ = Nothing :: Maybe ()
