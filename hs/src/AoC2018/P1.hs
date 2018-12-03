module AoC2018.P1 where

import Universum

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.IntSet (member,insert)

runPt1 :: Text -> IO ()
runPt1 = print . sum . parseInput

runPt2 :: Text -> IO ()
runPt2 = print . findRepeatedFreq . parseInput

type Input = [Int]

type Parser = Parsec Void Text

inputParser :: Parser Input
inputParser = L.signed space L.decimal `endBy` newline

parseInput :: Text -> Input
parseInput input =
  case parse inputParser "" input of
    Left bundle -> error . toText $ errorBundlePretty bundle
    Right p -> p

findRepeatedFreq :: [Int] -> Int
findRepeatedFreq = firstDupe mempty . scanl (+) 0 . cycle
  where
    firstDupe :: IntSet -> [Int] -> Int
    firstDupe seen (this:rest) =
      if this `member` seen
      then this
      else firstDupe (insert this seen) rest
