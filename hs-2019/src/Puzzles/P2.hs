module Puzzles.P2 where

import Puzzle

import IntcodeComputer


p2 :: Puzzle
p2 = Puzzle "2" inputParser pt1 pt2

type Input = Tape

inputParser :: Parser Input
inputParser = tapeParser

pt1 input = do
  Halted (Tape t) <- initProgram input
                     & (tape . ix 1 .~ 12)
                     & (tape . ix 2 .~ 2)
                     & runProgram
                     & pure
  t ^? ix 0

pt2 input = Just $ do
  noun <- [30..80]
  verb <- [30..80]
  Halted (Tape t) <- initProgram input
                     & tape . ix 1 .~ noun
                     & tape . ix 2 .~ verb
                     & runProgram
                     & pure
  result <- t ^.. ix 0
  guard $ 19690720 == result
  pure $ 100 * noun + verb

  -- where
  --   findAnswer = headMay [100 * noun + verb | noun <- [0..100], verb <- [0..100], checkValues noun verb st]
  --   checkValues noun verb = (Just 19690720 ==) . (getAnswer <=< runProgram) . updateTape noun verb
  --   updateTape noun verb =
  --   getAnswer = preview $ tape . ix 0
