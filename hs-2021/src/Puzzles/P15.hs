{-# LANGUAGE QuasiQuotes #-}
module Puzzles.P15 where

import AocPrelude
import Parse
import Puzzle

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Optics.TH

type Loc = (Int,Int)

p15 :: Puzzle
p15 = Puzzle "15" inputParser pt1 pt2

type Input = Map Loc Int

inputParser :: Parser Input
inputParser = coordinateMap <$> many singleDigitInt `endBy` newline

coordinateMap =
  ifoldMapOf
  (each % each %& icompose (flip (,)))
  Map.singleton

findShortestPath costMap = go (Set.singleton start) (Set.singleton (0, start))
  where
    xmin = 0
    xmax = maximum . fmap fst . Map.keys $ costMap
    ymin = 0
    ymax = maximum . fmap snd . Map.keys $ costMap

    start = (xmin,ymin)
    end = (xmax,ymax)

    go :: Set Loc -> Set (Int, Loc) -> Int
    go visited (Set.deleteFindMin -> ((visitingCost, visitingLoc@(vx, vy)), wavefront)) =
      if visitingLoc == end then visitingCost else
      let neighbors = do
            x <- [vx-1..vx+1]
            y <- if vx == x then [vy-1, vy+1] else [vy]
            let loc = (x,y)
                cost = visitingCost + costMap Map.! loc
            guard $
              xmin <= x && x <= xmax &&
              ymin <= y && y <= ymax &&
              Set.notMember loc visited
            pure (cost, loc)
      in go (Set.insert visitingLoc visited) (foldr Set.insert wavefront neighbors)

pt1 = Just . findShortestPath

tile map = Map.unions $ do
  x <- [0..4]
  y <- [0..4]
  map
    & Map.mapKeys (first (+ x*xsize) . second (+ y*ysize))
    & fmap (wrap . (+ y) . (+ x))
    & pure
  where
    xsize = (1 +) . maximum . fmap fst . Map.keys $ map
    ysize = (1 +) . maximum . fmap snd . Map.keys $ map
    wrap = (1 +) . (`mod` 9) . subtract 1

pt2 = Just . findShortestPath . tile

