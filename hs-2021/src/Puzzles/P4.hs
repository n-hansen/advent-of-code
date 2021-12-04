module Puzzles.P4 where

import AocPrelude hiding (some)
import Parse
import Puzzle

p4 :: Puzzle
p4 = Puzzle "4" inputParser pt1 pt2

data BingoBoard = BB [[(Int, Bool)]]
                deriving (Show)

type Input = ([Int], [BingoBoard])


drawnParser = unsignedInteger `sepBy1` string ","

boardParser = bingoBoard `sepBy1` newline
  where
    bingoBoard = BB <$> bingoRow `endBy1` newline
    bingoRow = many (string " ") >> ((, False) <$> unsignedInteger) `sepBy1` some (string " ")

inputParser :: Parser Input
inputParser = do
  drawn <- drawnParser
  newline
  newline
  boards <- boardParser
  return (drawn, boards)

scoreNextWinner (d:ds) boards = let boards' = fmap (mark d) boards
                                in case find checkWinner boards' of
                                     Nothing -> scoreNextWinner ds boards'
                                     Just w -> score d w

checkWinner (BB b) =
  let marks = fmap (fmap snd) b
  in any and marks || any and (transpose marks)

mark number (BB b) =
  BB $ fmap (fmap (\(n,m) -> (n, m || n == number))) b

score number (BB board) =
  (number *)
  . sum
  . sort
  . fmap fst
  . filter (not . snd)
  . mconcat
  $ board

pt1 = Just . uncurry scoreNextWinner

scoreFinalLoser (d:ds) boards =
  case filter (not . checkWinner) . fmap (mark d) $ boards of
    [finalLoser] -> scoreNextWinner ds [finalLoser]
    remaining -> scoreFinalLoser ds remaining

pt2 = Just . uncurry scoreFinalLoser
