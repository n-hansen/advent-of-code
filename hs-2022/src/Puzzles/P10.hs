{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P10 where

import AocPrelude hiding (cycle)
import Parse
import Puzzle

import Control.Lens.TH
import Control.Monad.RWS.Strict
import Data.List.Extra (chunksOf)
import qualified Data.Text as T

data CpuState = St { _cycle :: Int
                   , _register :: Int
                   } deriving (Eq,Show)
makeLenses ''CpuState

data Instruction
  = Noop
  | AddX Int
  deriving (Eq,Show)

p10 :: Puzzle
p10 = Puzzle "10" inputParser1 inputParser2 pt1 pt2

type Input1 = [Instruction]
type Input2 = Input1

inputParser1 :: Parser Input1
inputParser1 = instr `endBy1` newline
  where
    instr = noop <|> addx
    noop = string "noop" $> Noop
    addx = string "addx " >> AddX <$> signedInteger

inputParser2 :: Parser Input2
inputParser2 = inputParser1

run :: [Instruction] -> [CpuState]
run = go initialState
  where
    initialState = St 1 1
    go _ [] = []
    go st (Noop:rest) = st : go (st & cycle %~ (+ 1)) rest
    go st (AddX x:rest) =
      st : (st & cycle %~ (+ 1)) : go (st & cycle %~ (+ 2) & register %~ (+ x)) rest

signalStrength st = st ^. cycle * st ^. register

importantCycles = filter (view $ cycle . to (`elem` [20, 60, 100, 140, 180, 220]))

pt1 = Just . sum . fmap signalStrength . importantCycles . run

render :: [CpuState] -> Text
render sts = sts
  & fmap drawPx
  & chunksOf screenWidth
  & fmap T.pack
  & T.unlines

  where
    screenWidth = 40
    drawPx st =
      if abs (st ^. currentPx - st ^. register) <= 1
      then '#'
      else '.'
    currentPx = cycle . to (flip mod screenWidth . subtract 1)

pt2 = Just . render . run
