module Puzzles.P2 where

import AocPrelude
import Parse
import Puzzle

p2 :: Puzzle
p2 = Puzzle "2" inputParser1 inputParser2 pt1 pt2

type Input = [(RPS,RPS)]

data RPS = Rock | Paper | Scissors deriving (Eq,Show)
data Result = Win | Loss | Tie deriving (Eq,Show)

instance Pretty RPS where
  pretty = viaShow

inputParser1 :: Parser [(RPS,RPS)]
inputParser1 = flip sepEndBy1 newline $ do
  opponent <- choice [ string "A" $> Rock
                     , string "B" $> Paper
                     , string "C" $> Scissors
                     ]
  void space1
  me <- choice [ string "X" $> Rock
               , string "Y" $> Paper
               , string "Z" $> Scissors
               ]
  pure (opponent, me)


inputParser2 :: Parser [(RPS,Result)]
inputParser2 = flip sepEndBy1 newline $ do
  opponent <- choice [ string "A" $> Rock
                     , string "B" $> Paper
                     , string "C" $> Scissors
                     ]
  void space1
  me <- choice [ string "X" $> Loss
               , string "Y" $> Tie
               , string "Z" $> Win
               ]
  pure (opponent, me)

pt1 :: Input -> Maybe Int
pt1 = Just . sum . fmap (liftM2 (+) (outcomePoints . matchResult) (choicePoints . snd))

outcomePoints Win = 6
outcomePoints Tie = 3
outcomePoints Loss = 0

matchResult (x,y) | x == y = Tie
matchResult (Rock,Paper) = Win
matchResult (Paper,Scissors) = Win
matchResult (Scissors,Rock) = Win
matchResult _ = Loss

choicePoints Rock = 1 :: Int
choicePoints Paper = 2
choicePoints Scissors = 3

pt2 = Just . sum . fmap (liftM2 (+) (outcomePoints . snd) (choicePoints . uncurry findMove))

findMove opponent result = unsafeHead $ do
  myMove <- [Rock, Paper, Scissors]
  guard $ result == matchResult (opponent, myMove)
  pure myMove
