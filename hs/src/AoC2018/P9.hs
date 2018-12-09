{-# LANGUAGE NamedFieldPuns #-}
module AoC2018.P9 (p9) where

import           AoC2018
import           Universum                  hiding (uncons)

import           Deque
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

p9 :: Puzzle
p9 = Puzzle "9" inputParser (pure pt1) (pure pt2)

data Game = Game { nPlayers   :: Int
                 , lastMarble :: Int
                 } deriving (Show)

inputParser :: Parser Game
inputParser = Game
              <$> L.decimal
              <*  string " players; last marble is worth "
              <*> L.decimal
              <*  string " points"

data GameState = GS { scores          :: !(Deque Int)
                    , circle          :: !(Deque Int)
                    , unplayedMarbles :: [Int]
                    } deriving (Show)

instance Container (Deque a)

rotate :: Int -> Deque a -> Deque a
rotate 0 xs             = xs
rotate n xs | n > 0     = rotate (n-1) (shiftLeft xs)
            | otherwise = rotate (n+1) (shiftRight xs)

playGame :: Game -> Deque Int
playGame Game{nPlayers,lastMarble} = go initialState
  where
    initialState = GS
                   (fromList $ replicate nPlayers 0)
                   (pure 0)
                   [1..lastMarble]
    go (GS scores _ []) = scores
    go (GS scores circle (m:ms)) =
      if m `mod` 23 == 0
      then
        let
          Just (m2, circle') = uncons . rotate (-7) $ circle
          Just (score,scores') = uncons scores
        in
          go $ GS (snoc (score + m + m2) scores') circle' ms
      else
        go $ GS (rotate 1 scores) (cons m $ rotate 2 circle) ms

pt1 :: Game -> Text
pt1 = show . maximum . playGame

pt2 :: Game -> Text
pt2 = pt1 . modifyGame
  where
    modifyGame g@Game{lastMarble} = g { lastMarble = lastMarble * 100 }
