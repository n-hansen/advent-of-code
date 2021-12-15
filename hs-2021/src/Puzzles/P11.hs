{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Puzzles.P11 where

import AocPrelude hiding ((<+>))
import Parse
import Puzzle

import Control.Monad.Loops
import qualified Data.Map.Strict as Map
import Optics.TH
import Optics.State.Operators

import Prettyprinter

data SimulationState = St { _completedSteps :: Int
                          , _energyMap :: Map (Int,Int) Int
                          , _flashedThisStep :: [(Int,Int)]
                          , _flashCount :: Int
                          } deriving Show
makeLenses ''SimulationState

instance Pretty SimulationState where
  pretty st =
    "Step: " <+> st ^. completedSteps % to pretty
    <> line <>
    "Flashes: " <+> st ^. flashCount % to pretty
    <> line <>
    ( let (xMax,yMax) = maximum $ Map.keys $ st ^. energyMap
      in repeat [0..xMax]
         & zip [yMax,yMax-1..0]
         & foldr (\(y, xs) doc ->
                    doc <> hardline <>
                    foldMap (\x -> fromMaybe "_" $ st ^? energyMap % ix (x,y) % to pretty) xs
                 ) mempty

    )


p11 :: Puzzle
p11 = Puzzle "11" inputParser pt1 pt2

type Input = Map (Int,Int) Int

inputParser :: Parser Input
inputParser = coordinateMap <$> many singleDigitInt `endBy` newline

coordinateMap =
  ifoldMapOf
  (each % each %& icompose (flip (,)))
  Map.singleton

runSimulation :: Int -> State SimulationState ()
runSimulation steps = whileM_ condition loop
  where
    condition = (< steps) <$> use completedSteps
    loop = do
      increaseEnergy
      fireFlashes
      cleanup
    increaseEnergy =
      energyMap % each %= (+ 1)
    fireFlashes = do
      (flashed,notFlashed) <- use $ energyMap % to (Map.partition (> 9))
      when (not $ null flashed) $ do
        flashedThisStep %= (<> Map.keys flashed)
        energyMap .= notFlashed
        flashCount %= (+ length flashed)
        traverse_
          (\loc -> energyMap % ix loc %= (+ 1))
          (Map.keys flashed >>= neighbors)
        fireFlashes
    cleanup = do
      toReset <- use flashedThisStep
      flashedThisStep .= []
      energyMap %= ((<>) . Map.fromList $ zip toReset (repeat 0))
      completedSteps %= (+ 1)
    neighbors (x,y) = do
      x' <- [x-1..x+1]
      y' <- [y-1..y+1]
      guard $ x /= x' || y /= y'
      pure (x',y')

initialState em = St 0 em [] 0

pt1 =
  Just
  . view flashCount
  . execState (runSimulation 100)
  . initialState

runSimulationUntilBigFlash :: State SimulationState Int
runSimulationUntilBigFlash = startLoop
  where
    startLoop = do
      increaseEnergy
      fireFlashes
    increaseEnergy =
      energyMap % each %= (+ 1)
    fireFlashes = do
      (flashed,notFlashed) <- use $ energyMap % to (Map.partition (> 9))
      if | null notFlashed -> use $ completedSteps % to (+ 1)
         | not (null flashed) -> do
              flashedThisStep %= (<> Map.keys flashed)
              energyMap .= notFlashed
              flashCount %= (+ length flashed)
              traverse_
                (\loc -> energyMap % ix loc %= (+ 1))
                (Map.keys flashed >>= neighbors)
              fireFlashes
         | otherwise -> cleanup
    cleanup = do
      toReset <- use flashedThisStep
      flashedThisStep .= []
      energyMap %= ((<>) . Map.fromList $ zip toReset (repeat 0))
      completedSteps %= (+ 1)
      startLoop
    neighbors (x,y) = do
      x' <- [x-1..x+1]
      y' <- [y-1..y+1]
      guard $ x /= x' || y /= y'
      pure (x',y')

pt2 =
  Just
  . evalState runSimulationUntilBigFlash
  . initialState


test = pretty
       . execState (runSimulation 2)
       . initialState
       $ parsed
  where
    input = [r|5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
|]
    parsed = fromMaybe undefined $ parseMaybe inputParser input

