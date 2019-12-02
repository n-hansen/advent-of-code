{-# LANGUAGE NamedFieldPuns #-}
module AoC2018 ( Puzzle(Puzzle, puzzleId)
               , Parser
               , solvePuzzle
               ) where

import           Universum

import           Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void Text

data Puzzle = forall input.
              Puzzle { puzzleId    :: String -- ¯\_(ツ)_/¯
                     , inputParser :: Parser input
                     , pt1         :: Maybe (input -> Text)
                     , pt2         :: Maybe (input -> Text)
                     }

solvePuzzle :: Puzzle -> IO Text
solvePuzzle Puzzle{puzzleId, inputParser, pt1, pt2} =
  (header <>) . puzzleSolution . parseInput <$> readInput
  where
    header = "--Puzzle " <> toText puzzleId <> "--\n"
    inputFilename = "inputs/" <> puzzleId
    readInput = readFile inputFilename
    puzzleSolution = solutionPrinter pt1 pt2
    parseInput input =
      case parse inputParser inputFilename input of
        Left bundle  -> error . toText $ errorBundlePretty bundle
        Right parsed -> parsed

solutionPrinter :: Maybe (input -> Text) -> Maybe (input -> Text) -> input -> Text
solutionPrinter pt1ptr pt2ptr input = unlines . catMaybes $ [pt1, pt2]
  where
    pt1 = unlines . ("---Part 1---":) . pure <$> (pt1ptr <*> pure input)
    pt2 = unlines . ("---Part 2---":) . pure <$> (pt2ptr <*> pure input)
