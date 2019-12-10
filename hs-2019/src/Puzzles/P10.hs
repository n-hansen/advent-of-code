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

losAngle = fmap reduce . offset
  where
    offset = uncurry bimap <$> over both subtract
    reduce = over both . flip div =<< uncurry gcd

pt1 input = Just $ maximum . fmap (countInLOS input) $ input

pt2 input = headMay . drop 199 $ vaporizeOrder
  where
    station@(x,y) = maximumOn (countInLOS input) input
    vaporizeOrder = mconcat
                    . transpose
                    . fmap (sortOn (uncurry (+) . bimap (abs . subtract x) (abs . subtract y)) . snd)
                    . sortOn (negate . uncurry (atan2 `on` fromIntegral) . fst)
                    . groupSort
                    . fmap (losAngle station &&& identity)
                    . filter (/= station)
                    $ input
