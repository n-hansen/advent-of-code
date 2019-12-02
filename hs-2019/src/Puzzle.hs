module Puzzle
  ( Puzzle(Puzzle, puzzleId)
  , Parser
  , solvePuzzle
  ) where

import Data.Text (unlines)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void Text

data Puzzle = forall input result1 result2.
              (Show result1, Show result2) =>
              Puzzle { puzzleId    :: Text -- ¯\_(ツ)_/¯
                     , inputParser :: Parser input
                     , pt1         :: Maybe (input -> result1)
                     , pt2         :: Maybe (input -> result2)
                     }

solvePuzzle :: Puzzle -> IO ()
solvePuzzle Puzzle{puzzleId, inputParser, pt1, pt2} = do
  putText $ "--- Puzzle " <> puzzleId <> " ---"
  let inputFilename = "inputs/" <> toS puzzleId
  rawInput <- readFile inputFilename
  case parse inputParser inputFilename rawInput of
    Left bundle -> do
      putText "/!\\ Parse Error /!\\"
      putStrLn $ errorBundlePretty bundle
    Right parsed -> do
      let display _ Nothing = pure ()
          display txtName (Just result) = do
            putText $ " -- " <> txtName <> " --"
            putText $ show result
      sequence pt1 parsed & display "Part 1"
      sequence pt2 parsed & display "Part 2"
