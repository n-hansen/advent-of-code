{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P7 where

import Parse
import Puzzle

import           Data.Vector (Vector)
import qualified Data.Vector as V


data ComputerState = CS { _position :: Int
                        , _inputStream :: [Int]
                        , _outputStream :: [Int]
                        , _tape :: Vector Int
                        } deriving (Eq, Show)
makeLenses ''ComputerState

inputParser :: Parser Input
inputParser = (flip $ CS 0 `flip` []) . V.fromList <$> (signedInteger `sepBy` "," <* optional newline)



p7 :: Puzzle
p7 = Puzzle "7" inputParser pt1 pt2

type Input = [Int] -> ComputerState

pt1 :: Input -> Maybe Int
pt1 input = Just . maximum $ do
  phaseConfig <- permutations [0..4]
  pure $ foldr runAmp 0 phaseConfig
  where
    runAmp phaseSetting ampInput =
      views outputStream unsafeHead . runProgram $ (input [phaseSetting, ampInput])


pt2 input = Just . maximum $ do
  phaseConfig <- permutations [5..9]
  pure $ runFeedbackLoop input phaseConfig


runProgram st = let (status, st') = stepProgram st in if status == Continue then runProgram st' else st'

runUntilBlocked st =
  case stepProgram st of
    (Continue, st') -> runUntilBlocked st'
    ret -> ret

runFeedbackLoop input phaserConfig = go [0] initAmps
  where
    initAmps = zipWith (\id phaserSetting -> (id, input [phaserSetting])) [0..] phaserConfig
    go prevOutputs ((id,st):amps) =
      case st & inputStream %~ (<> prevOutputs) & runUntilBlocked of
        (Done, st') -> if id == 4 then st' ^? outputStream . ix 0
                       else let nextOutputs = view outputStream st'
                                st'' = st' & outputStream .~ mempty
                            in go nextOutputs amps
        (WaitForInput, st') -> let nextOutputs = view outputStream st'
                                   st'' = st' & outputStream .~ mempty
                               in go nextOutputs (amps <> [(id,st'')])

data RunStatus = Continue | WaitForInput | Done
               deriving (Eq,Show)

stepProgram :: ComputerState -> (RunStatus, ComputerState)
stepProgram st = eval . destructOpcode . unsafeFromJust $ arg 0
  where
    arg n = st ^? tape . ix (st ^. position . to (+ n))
    deref i = st ^? tape . ix i
    withMode 0 = deref
    withMode 1 = Just . identity
    advance n = position %~ (+ n)
    writeOutput x = outputStream %~ (x:)
    popInput = inputStream %~ unsafeTail
    continue = (Continue,)
    waitForInput = (WaitForInput, st)

    eval :: (Int, [Int]) -> (RunStatus, ComputerState)
    eval (1, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ val1 + val2
        & advance 4
        & continue

    eval (2, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ val1 * val2
        & advance 4
        & continue

    eval (3, 0:_)
      | Just out <- arg 1
      , Just _   <- deref out
      , Just val <- st ^? inputStream . ix 0
      = st
        & popInput
        & tape . ix out .~ val
        & advance 2
        & continue

    eval (3, _)
      | Nothing <- st ^? inputStream . ix 0
      = waitForInput

    eval (4, mode:_)
      | Just val <- withMode mode =<< arg 1
      = st
        & writeOutput val
        & advance 2
        & continue

    eval (5, mode1:mode2:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      = st
        & (if val1 /= 0 then position .~ val2 else advance 3)
        & continue

    eval (6, mode1:mode2:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      = st
        & (if val1 == 0 then position .~ val2 else advance 3)
        & continue

    eval (7, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ (if val1 < val2 then 1 else 0)
        & advance 4
        & continue

    eval (8, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ (if val1 == val2 then 1 else 0)
        & advance 4
        & continue

    eval (99, _) = (Done,st)

    eval (o,ms) = error $ show (o,ms,st)

destructOpcode opcode = (op, argModes <> repeat 0)
  where
    op = opcode `mod` 100
    argModes = unfoldr (\number -> if number > 0
                                   then Just (number `mod` 10, number `div` 10)
                                   else Nothing)
               $ opcode `div` 100
