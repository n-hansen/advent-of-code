module Puzzles.P9 where

import AocPrelude
import Parse
import Puzzle

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
    riskLevel = (+ 1)
    lowPoints :: IxFold (Int,Int) HeightMap Int
    lowPoints =
      ifolded %& ifiltered (\loc height -> all (> height) . neighborsOf input $ loc)

neighborsOf heightMap (x,y) = do
  x' <- [x-1..x+1]
  y' <- if x' == x then [y-1,y+1] else [y]
  maybeToList $ Map.lookup (x',y') heightMap


data FillColor = Filled Int
               | NeverFilled
               | Pending
               deriving (Ord,Eq,Show)

fill :: HeightMap -> Map (Int,Int) FillColor
fill heightMap = go 0 init
  where
    lowPoints :: Set (Int,Int)
    lowPoints =
      Set.fromList
      . fmap fst
      . itoListOf (ifolded %& ifiltered (\loc height -> all (> height) . neighborsOf heightMap $ loc))
      $ heightMap
    init :: Map (Int,Int) FillColor
    init =
      Pending <$ heightMap
      & partsOf (itraversed %& indices (`Set.member` lowPoints)) .~ (fmap Filled [0..])
    go :: Int -> Map (Int,Int) FillColor -> Map (Int,Int) FillColor
    go 9 filled = filled
    go lv filled = go (lv + 1) $ foldr (<>) filled $ do
      (loc, height) <- Map.toList heightMap
      guard $ height == lv
      guard $ Pending == filled Map.! loc
      case filter (/= Pending) . ordNub $ neighborsOf filled loc of
        [Filled c] -> [Map.singleton loc (Filled c)]
        _ -> [Map.singleton loc NeverFilled]


pt2 =
  Just
  . product
  . take 3
  . reverse
  . sort
  . fmap length
  . group
  . sort
  . mapMaybe (\case
                 Filled x -> Just x
                 _ -> Nothing
             )
  . Map.elems
  . fill


