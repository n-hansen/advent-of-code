module Puzzles.P9 where

import AocPrelude
import Parse
import Puzzle

import qualified Data.Map.Strict as Map

p9 :: Puzzle
p9 = Puzzle "9" inputParser pt1 pt2

type HeightMap = Map (Int,Int) Int

inputParser :: Parser HeightMap
inputParser = coordinateMap <$> many singleDigitInt `endBy` newline

coordinateMap =
  ifoldMapOf
  (each % each %& icompose (,))
  Map.singleton

pt1 input =
  Just $ sumOf (lowPoints % to riskLevel) input
  where
    lowPoints :: IxFold (Int,Int) Input Int
    lowPoints =
      ifolded %& ifiltered (\(x,y) height ->
                               and $ do
                                x' <- [x-1..x+1]
                                y' <- if x' == x then [y-1,y+1] else [y]
                                height' <- maybeToList $ Map.lookup (x',y') input
                                pure $ height' > height
                           )
    riskLevel = (+ 1)

pt2 _ = Nothing :: Maybe ()
