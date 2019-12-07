{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P5 where

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

p5 :: Puzzle
p5 = Puzzle "5" inputParser pt1 pt2

type Input = [Int] -> ComputerState

inputParser :: Parser Input
inputParser = (flip $ CS 0 `flip` []) . V.fromList <$> (signedInteger `sepBy` "," <* optional newline)

pt1 input = Just $ preview (outputStream . ix 0) =<< runProgram (input [1])

pt2 input = Just $ preview (outputStream . ix 0) =<< runProgram (input [5])


runProgram :: ComputerState -> Maybe ComputerState
runProgram st = arg 0 >>= (eval . destructOpcode)
  where
    arg n = st ^? tape . ix (st ^. position . to (+ n))
    deref i = st ^? tape . ix i
    withMode 0 = deref
    withMode 1 = Just . identity
    advance n = position %~ (+ n)
    writeOutput x = outputStream %~ (x:)
    popInput = inputStream %~ unsafeTail

    eval :: (Int, [Int]) -> Maybe ComputerState
    eval (1, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ val1 + val2
        & advance 4
        & runProgram

    eval (2, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ val1 * val2
        & advance 4
        & runProgram

    eval (3, 0:_)
      | Just out <- arg 1
      , Just _   <- deref out
      , Just val <- st ^? inputStream . ix 0
      = st
        & popInput
        & tape . ix out .~ val
        & advance 2
        & runProgram

    eval (4, mode:_)
      | Just val <- withMode mode =<< arg 1
      = st
        & writeOutput val
        & advance 2
        & runProgram

    eval (5, mode1:mode2:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      = st
        & (if val1 /= 0 then position .~ val2 else advance 3)
        & runProgram

    eval (6, mode1:mode2:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      = st
        & (if val1 == 0 then position .~ val2 else advance 3)
        & runProgram

    eval (7, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ (if val1 < val2 then 1 else 0)
        & advance 4
        & runProgram

    eval (8, mode1:mode2:0:_)
      | Just val1 <- withMode mode1 =<< arg 1
      , Just val2 <- withMode mode2 =<< arg 2
      , Just out  <- arg 3
      , Just _    <- deref out
      = st
        & tape . ix out .~ (if val1 == val2 then 1 else 0)
        & advance 4
        & runProgram

    eval (99, _) = pure st

    eval (o,ms) = error $ show (o,ms,st)

destructOpcode opcode = (op, argModes <> repeat 0)
  where
    op = opcode `mod` 100
    argModes = unfoldr (\number -> if number > 0
                                   then Just (number `mod` 10, number `div` 10)
                                   else Nothing)
               $ opcode `div` 100
