module Puzzles.P8 where

import AocPrelude
import Parse
import Puzzle

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

p8 :: Puzzle
p8 = Puzzle "8" inputParser1 inputParser2 pt1 pt2

type Input1 = [[Int]]
type Input2 = Input1

inputParser1 :: Parser Input1
inputParser1 = row `endBy1` newline
  where
    row = many singleDigitInt

inputParser2 :: Parser Input2
inputParser2 = inputParser1

annotateWithCoords :: [[Int]] -> [[((Int,Int),Int)]]
annotateWithCoords = fmap annotateRow . zip [0..]
  where
    annotateRow (rowIx, row) = fmap (annotateCell rowIx) . zip [0..] $ row
    annotateCell rowIx (colIx, cell) = ((rowIx,colIx), cell)

checkSingleRowVisibility :: (Ord a, Bounded a) => [(a, Int)] -> Set a
checkSingleRowVisibility = snd . foldl' checkVisibility (minBound,mempty)
  where
    checkVisibility acc@(tallestYet, vis) (x,height)
      | height > tallestYet = (height, Set.insert x vis)
      | otherwise = acc

checkSingleDirectionVisibility = foldMap checkSingleRowVisibility

findVisible :: (Ord a, Bounded a) => [[(a, Int)]] -> Set a
findVisible forest = foldMap checkSingleDirectionVisibility [eastward, westward, northward, southward]
  where
    eastward = forest
    westward = reverse <$> eastward
    southward = transpose eastward
    northward = reverse <$> southward

pt1 = Just . length . findVisible . annotateWithCoords

computeScenery :: Map (Int,Int) Int -> (Int,Int) -> Int
computeScenery heights loc = westward * eastward * northward * southward
  where
    westward = scanScenery $ _2 %~ subtract 1
    eastward = scanScenery $ _2 %~ (+ 1)
    northward = scanScenery $ _1 %~ subtract 1
    southward = scanScenery $ _1 %~ (+ 1)

    Just height = heights ^. at loc

    scanScenery step = scanScenery' step 0 (step loc)
    scanScenery' step dist l =
      case heights ^. at l of
        Nothing -> dist
        Just h | h >= height -> dist + 1
               | otherwise -> scanScenery' step (dist + 1) (step l)

pt2 input = Just answer
  where
    heights = Map.fromList . mconcat . annotateWithCoords $ input
    locs = Map.keys heights
    notEdge (x,y) = x > 0 && y > 0 && x < (maximum . fmap fst $ locs) && y < (maximum . fmap snd $ locs)
    answer = maximum
             . fmap (computeScenery heights)
             . filter notEdge
             $ locs
