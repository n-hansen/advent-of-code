module AoC2018.P5 (p5) where

import           AoC2018
import           Universum            hiding (many, reduce)

import           Data.Char
import           Text.Megaparsec
import           Text.Megaparsec.Char

p5 :: Puzzle
p5 = Puzzle "5" inputParser (pure pt1) mempty

data Unit = Positive Char
          | Negative Char
          deriving (Show,Eq)

type Input = [Unit]

inputParser :: Parser Input
inputParser = many unit
  where
    unit :: Parser Unit
    unit = positive <|> negative
    positive = Positive <$> lowerChar
    negative = Negative . toLower <$> upperChar

cancel :: Unit -> Unit -> Bool
cancel (Positive p) (Negative n) = p == n
cancel (Negative n) (Positive p) = p == n
cancel _ _                       = False

reduce :: [Unit] -> [Unit]
reduce = go []
  where
    go result [] = result
    go [] (u:us) = go [u] us
    go (u:us) (v:vs) | cancel u v = go us vs
                     | otherwise  = go (v:u:us) vs

pt1 :: Input -> Text
pt1 = show . length . reduce
