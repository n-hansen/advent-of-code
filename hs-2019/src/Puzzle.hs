module Puzzle
  ( Puzzle(Puzzle, puzzleId)
  , Parser
  , solvePuzzle
  ) where

import Data.Text (unlines)
import Text.Megaparsec (Parsec, errorBundlePretty, parse)

type Parser = Parsec Void Text

data Puzzle = forall input.
              Puzzle { puzzleId    :: Text -- ¯\_(ツ)_/¯
                     , inputParser :: Parser input
                     , pt1         :: Maybe (input -> Text)
                     , pt2         :: Maybe (input -> Text)
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
      let run _ Nothing = pure ()
          run txtName (Just pt) = do
            putText $ " -- " <> txtName <> " --"
            putText $ pt parsed
      run "Part 1" pt1
      run "Part 2" pt2
