module Puzzles.Template where

import Parse
import Puzzle

pPUZZLE :: Puzzle
pPUZZLE = Puzzle "PUZZLE" inputParser pt1 pt2

type Input = Text

inputParser :: Parser Input
inputParser = pure ""

pt1 _ = Nothing :: Maybe ()

pt2 _ = Nothing :: Maybe ()
