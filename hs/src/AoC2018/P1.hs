module AoC2018.P1 where

import Universum

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

runPt1 :: Text -> IO ()
runPt1 = print . sum . (:) 0 . parseInput

type Input = [Int]

type Parser = Parsec Void Text

inputParser :: Parser Input
inputParser = L.signed space L.decimal `endBy` newline

parseInput :: Text -> Input
parseInput input =
  case parse inputParser "" input of
    Left bundle -> error . toText $ errorBundlePretty bundle
    Right p -> p
