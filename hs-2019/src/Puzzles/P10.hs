module Puzzles.P10 where

import Parse
import Puzzle

import Data.List.Extra hiding (sortOn)

p10 :: Puzzle
p10 = Puzzle "10" inputParser pt1 pt2

type Input = [Coord]

type Coord = (Int,Int)

inputParser :: Parser Input
inputParser = gridToCoords <$> parseGrid <* optional newline
  where
    parseGrid :: Parser [[Bool]]
    parseGrid = parseRow `sepBy` newline
    parseRow :: Parser [Bool]
    parseRow = many asteroid
    asteroid :: Parser Bool
    asteroid = (string "." >> pure False) <|> (string "#" >> pure True)
    gridToCoords :: [[Bool]] -> [Coord]
    gridToCoords grid = do
      (y,row) <- zip [0..] grid
      (x,hasAsteroid) <- zip [0..] row
      guard hasAsteroid
      pure (x,y)

countInLOS grid me = length . nubOrdOn (losAngle me) . filter (/= me) $ grid

losAngle (myX,myY) (theirX,theirY) = (dx `div` factor, dy `div` factor)
  where
    dx = theirX - myX
    dy = theirY - myY
    factor = gcd dx dy

pt1 input = Just $ maximum . fmap (countInLOS input) $ input

pt2 input = headMay . drop 199 $ vaporizeOrder
  where
    station = maximumOn (countInLOS input) input
    asteroids = filter (/= station) input
    vaporizeOrder = mconcat
                    . transpose
                    . fmap (sortOn (distance station) . snd)
                    . sortOn (negate . uncurry atan2 . bimap fromIntegral fromIntegral . fst)
                    . groupSort
                    . fmap (losAngle station &&& identity)
                    $ asteroids
    distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)
