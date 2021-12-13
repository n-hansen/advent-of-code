module Puzzles.P10 where

import AocPrelude
import Parse
import Puzzle

data ChunkChar = Open ChunkType | Close ChunkType deriving (Eq,Show)
data ChunkType = Paren | Square | Curly | Angle deriving (Eq,Show)

data ParseResult = Corrupted ChunkType
                 | Incomplete [ChunkType]
                 deriving (Eq,Show)

p10 :: Puzzle
p10 = Puzzle "10" inputParser pt1 pt2

type Input = [[ChunkChar]]

inputParser :: Parser Input
inputParser = line `endBy` newline
  where
    line = many chunkChar
    chunkChar = choice [ Open Paren <$ string "("
                       , Close Paren <$ string ")"
                       , Open Square <$ string "["
                       , Close Square <$ string "]"
                       , Open Curly <$ string "{"
                       , Close Curly <$ string "}"
                       , Open Angle <$ string "<"
                       , Close Angle <$ string ">"
                       ]

parseLine :: [ChunkChar] -> ParseResult
parseLine = go []
  where
    go os [] = Incomplete os
    go os (Open t:rest) = go (t:os) rest
    go [] (Close _:_) = panic "Unexpected close"
    go (o:os) (Close t:rest) | o == t = go os rest
                             | otherwise = Corrupted t

pt1 =
  Just
  . sum
  . fmap points
  . mapMaybe (\case
                 Corrupted t -> Just t
                 _ -> Nothing
             )
  . fmap parseLine
  where
    points Paren = 3 :: Int
    points Square = 57
    points Curly = 1197
    points Angle = 25137


pt2 =
  Just
  . middle
  . sort
  . fmap (score 0)
  . mapMaybe (\case
                 Incomplete os -> Just os
                 _ -> Nothing
             )
  . fmap parseLine
  where
    score tally [] = tally
    score tally (t:ts) =
      let pts = case t of
                  Paren -> 1 :: Int
                  Square -> 2
                  Curly -> 3
                  Angle -> 4
      in score (tally * 5 + pts) ts
    middle [x] = x
    middle (_ :< (xs :> _)) = middle xs
    middle _ = panic "I was promised an odd number of things!"
