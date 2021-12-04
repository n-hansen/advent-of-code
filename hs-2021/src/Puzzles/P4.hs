module Puzzles.P4 where

import AocPrelude hiding (some)
import Parse
import Puzzle

p4 :: Puzzle
p4 = Puzzle "4" inputParser pt1 pt2

data BingoBoard = BB [[(Int, Bool)]]
                deriving (Show)

type Input = ([Int], [BingoBoard])

numberListParser = unsignedInteger `sepBy1` string ","

boardParser = BB <$> bingoRow `endBy1` newline
  where
    bingoRow = do
      actualSpaces
      ((, False) <$> unsignedInteger) `sepBy1` actualSpaces1

inputParser :: Parser Input
inputParser = do
  drawn <- numberListParser
  newline
  newline
  boards <- boardParser `sepBy1` newline
  return (drawn, boards)

scoreNextWinner (d:ds) boards =
  case find checkWinner marked of
    Nothing -> scoreNextWinner ds $ marked
    Just w -> score d w
  where
    marked = markBoards d boards

markBoards d = fmap (mark d)
  where
    mark number (BB b) =
      BB $ fmap (fmap (\(n,m) -> (n, m || n == number))) b
checkWinner (BB b) =
  let marks = fmap (fmap snd) b
  in any and marks || any and (transpose marks)


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
  case filter (not . checkWinner) $ markBoards d boards of
    [finalLoser] -> scoreNextWinner ds [finalLoser]
    remaining -> scoreFinalLoser ds remaining

pt2 = Just . uncurry scoreFinalLoser
