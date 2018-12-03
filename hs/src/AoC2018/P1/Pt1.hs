module AoC2018.P1.Pt1 where

import Universum

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

run :: Text -> IO ()
run = print . sum . (:) 0 . parseInput

type Input = [Int]

type Parser = Parsec Void Text

inputParser :: Parser Input
inputParser = L.signed space L.decimal `endBy` newline

parseInput :: Text -> Input
parseInput input =
  case parse inputParser "" input of
    Left bundle -> error . toText $ errorBundlePretty bundle
    Right p -> p
