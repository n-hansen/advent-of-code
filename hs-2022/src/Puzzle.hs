module Puzzle
  ( Puzzle(Puzzle, puzzleId)
  , Parser
  , readPuzzleInput
  , solvePuzzle
  ) where

import AocPrelude

import Data.Text (unlines)
import Parse (Parser)
import Text.Megaparsec (errorBundlePretty, parse)

data Puzzle = forall input1 input2 result1 result2.
              (Pretty result1, Pretty result2) =>
              Puzzle { puzzleId    :: Text -- ¯\_(ツ)_/¯
                     , inputParser1 :: Parser input1
                     , inputParser2 :: Parser input2
                     , pt1         :: input1 -> Maybe result1
                     , pt2         :: input2 -> Maybe result2
                     }

readPuzzleInput pid = readFile $ "inputs/" <> toS pid

solvePuzzle :: Puzzle -> IO ()
solvePuzzle Puzzle{puzzleId, inputParser1, inputParser2, pt1, pt2} = do
  putText $ "--- Puzzle " <> puzzleId <> " ---"
  rawInput <- readPuzzleInput puzzleId
  let parsed = do
        parsed1 <- parse inputParser1 ("puzzle " <> toS puzzleId <> " input, part 1") rawInput
        parsed2 <- parse inputParser2 ("puzzle " <> toS puzzleId <> " input, part 2") rawInput
        pure (parsed1, parsed2)
  case parsed of
    Left bundle -> do
      putText "/!\\ Parse Error /!\\"
      putStrLn $ errorBundlePretty bundle
    Right (parsed1, parsed2) -> do
      let display _ Nothing = pure ()
          display txtName (Just result) = do
            putText $ " -- " <> txtName <> " --"
            putText . show . pretty $ result
      display "Part 1" $ pt1 parsed1
      display "Part 2" $ pt2 parsed2
