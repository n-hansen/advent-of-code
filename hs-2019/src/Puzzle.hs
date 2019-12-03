module Puzzle
  ( Puzzle(Puzzle, puzzleId)
  , Parser
  , solvePuzzle
  ) where

import Data.Text (unlines)
import Parse (Parser)
import Text.Megaparsec (errorBundlePretty, parse)

data Puzzle = forall input result1 result2.
              (Show result1, Show result2) =>
              Puzzle { puzzleId    :: Text -- ¯\_(ツ)_/¯
                     , inputParser :: Parser input
                     , pt1         :: input -> Maybe result1
                     , pt2         :: input -> Maybe result2
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
      display "Part 1" $ pt1 parsed
      display "Part 2" $ pt2 parsed
