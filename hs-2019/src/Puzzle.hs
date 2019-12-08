module Puzzle
  ( Puzzle(Puzzle, puzzleId)
  , Parser
  , readPuzzleInput
  , solvePuzzle
  ) where

import Data.Text (unlines)
import Parse (Parser)
import Text.Megaparsec (errorBundlePretty, parse)

data Puzzle = forall input result1 result2.
              (Pretty result1, Pretty result2) =>
              Puzzle { puzzleId    :: Text -- ¯\_(ツ)_/¯
                     , inputParser :: Parser input
                     , pt1         :: input -> Maybe result1
                     , pt2         :: input -> Maybe result2
                     }

readPuzzleInput pid = readFile $ "inputs/" <> toS pid

solvePuzzle :: Puzzle -> IO ()
solvePuzzle Puzzle{puzzleId, inputParser, pt1, pt2} = do
  putText $ "--- Puzzle " <> puzzleId <> " ---"
  rawInput <- readPuzzleInput puzzleId
  case parse inputParser ("puzzle " <> toS puzzleId <> " input") rawInput of
    Left bundle -> do
      putText "/!\\ Parse Error /!\\"
      putStrLn $ errorBundlePretty bundle
    Right parsed -> do
      let display _ Nothing = pure ()
          display txtName (Just result) = do
            putText $ " -- " <> txtName <> " --"
            putText . show . pretty $ result
      display "Part 1" $ pt1 parsed
      display "Part 2" $ pt2 parsed
