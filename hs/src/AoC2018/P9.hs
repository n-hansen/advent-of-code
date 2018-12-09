{-# LANGUAGE NamedFieldPuns #-}
module AoC2018.P9 (p9) where

import           AoC2018
import           Universum                  hiding (replicate)

import           Data.Sequence
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

p9 :: Puzzle
p9 = Puzzle "9" inputParser (pure pt1) mempty

data Game = Game { nPlayers   :: Int
                 , lastMarble :: Int
                 } deriving (Show)

inputParser :: Parser Game
inputParser = Game
              <$> L.decimal
              <*  string " players; last marble is worth "
              <*> L.decimal
              <*  string " points"

data GameState = GS { scores          :: Seq Int -- current player at left head
                    , circle          :: Seq Int -- current marble at left head, right=cw
                    , unplayedMarbles :: [Int]
                    } deriving (Show)

rotate :: Int -> Seq a -> Seq a
rotate _ Empty      = Empty
rotate 0 xs         = xs
rotate n (x :<| xs) | n > 0 = rotate (n-1) (xs |> x)
rotate n (xs :|> x) = rotate (n+1) (x <| xs)

playGame :: Game -> Seq Int
playGame Game{nPlayers,lastMarble} = go initialState
  where
    initialState = GS
                   (replicate nPlayers 0)
                   (singleton 0)
                   [1..lastMarble]
    go (GS scores _ []) = scores
    go (GS (score :<| scores) circle (m:ms)) =
      if m `mod` 23 == 0
      then
        let
          (m2 :<| circle') = rotate (-7) circle
        in
          go $ GS (scores |> score + m + m2) circle' ms
      else
        go $ GS (scores |> score) (m <| rotate 2 circle) ms

pt1 :: Game -> Text
pt1 = show . maximum . playGame
