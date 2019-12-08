module Puzzles.P7 where

import Puzzle

import IntcodeComputer


p7 :: Puzzle
p7 = Puzzle "7" inputParser pt1 pt2


type Input = Tape

inputParser = tapeParser


pt1 :: Input -> Maybe Int
pt1 input = Just . maximum $ do
  phaseConfig <- permutations [0..4]
  foldrM runAmp 0 phaseConfig
  where
    program = initProgram input
    runAmp phaseSetting ampInput =
      case program & provideInput [phaseSetting, ampInput] & runProgram of
        Halted (Output (o:_)) -> pure o


pt2 input = Just . maximum $ do
  phaseConfig <- permutations [5..9]
  pure $ runFeedbackLoop program phaseConfig
  where
    program = initProgram input

runFeedbackLoop program phaserConfig = go [0] initAmps
  where
    initAmps = zipWith
               (\id phaserSetting -> (id, program & provideInput [phaserSetting]))
               [0..]
               phaserConfig
    go prevOutputs ((id,st):amps) =
      case st & provideInput prevOutputs & runProgram of
        Halted (Output os@(o:_)) -> if id == 4 then Just o
                                    else go os amps
        WaitingForInput (DrainedOutput o st') -> go o (amps <> [(id,st')])
