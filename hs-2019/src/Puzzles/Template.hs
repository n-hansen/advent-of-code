module Puzzles.Template where

import Puzzle

pPUZZLE :: Puzzle
pPUZZLE = Puzzle "PUZZLE" inputParser pt1 pt2

type Input = Text

inputParser :: Parser Input
inputParser = pure ""

pt1 = Nothing :: Maybe (a -> a)

pt2 = Nothing :: Maybe (a -> a)
