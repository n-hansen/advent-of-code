{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P15 where

import Parse
import Puzzle

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.RWS.Strict

import IntcodeComputer

data RoverState = RS { _location :: (Int,Int)
                     , _travelTime :: Int
                     , _computerState :: ComputerState
                     , _done :: Bool
                     } deriving Show
makeLenses ''RoverState

p15 :: Puzzle
p15 = Puzzle "15" inputParser pt1 pt2

type Input = Tape

inputParser :: Parser Input
inputParser = tapeParser

bfs :: (Monad m)
    => (a -> m [a])
    -> (a -> Bool)
    -> a
    -> m (Maybe a)
bfs expand test init = go [init]
  where
    go [] = pure Nothing
    go xs =
      case getFirst $ foldMap (\x -> First $ if test x then Just x else Nothing) xs of
        Just x -> pure $ Just x
        Nothing -> go =<< mconcat <$> traverse expand xs

nextSteps :: MonadState (Set (Int,Int)) m => RoverState -> m [RoverState]
nextSteps st = do
  alreadySeen <- get
  modify $ Set.insert (st ^. location)
  let nextMoves = do
        cmd <- [1..4]
        let updateLoc = case cmd of
                          -- N
                          1 -> location . _2 +~ 1
                          -- S
                          2 -> location . _2 -~ 1
                          -- W
                          3 -> location . _1 -~ 1
                          -- E
                          4 -> location . _1 +~ 1
        guard
          $ views location (not . flip Set.member alreadySeen) $ updateLoc st
        pure (cmd, updateLoc)
  catMaybes <$> traverse runMove nextMoves

  where
    runMove (cmd, updateLoc) = do
      let st' = st & updateLoc & travelTime +~ 1
      pure $
        case runProgram . provideInput [cmd] $ st' ^. computerState of
          WaitingForInput (Output [0]) -> Nothing
          WaitingForInput (DrainedOutput [1] cst) -> Just $ st' & computerState .~ cst
          WaitingForInput (DrainedOutput [2] cst) -> Just $ st' & computerState .~ cst & done .~ True
          _ -> Nothing

searchForOxygenSystem rs = evalState (bfs nextSteps (view done) rs) Set.empty

pt1 input = view travelTime <$> searchForOxygenSystem initRobot
  where
    initRobot = RS (0,0) 0 (initProgram input) False

newtype Maximum = Max { getMax :: Int } deriving (Show)
instance Semigroup Maximum where
  (Max x) <> (Max y) = Max $ max x y
instance Monoid Maximum where
  mempty = Max minBound

reportTravelTime rs = do
  tell $ rs ^. travelTime . to Max
  pure rs

pt2 input = Just . subtract (atO2 ^. travelTime) . getMax . snd $ evalRWS exploreFromOxygenSystem () Set.empty
  where
    exploreFromOxygenSystem = bfs (reportTravelTime >=> nextSteps) (const False) atO2
    Just atO2 = searchForOxygenSystem $ RS (0,0) 0 (initProgram input) False
