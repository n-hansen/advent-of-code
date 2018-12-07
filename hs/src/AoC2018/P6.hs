{-# LANGUAGE FlexibleContexts #-}
module AoC2018.P6 (p6) where

import           AoC2018
import           Universum

import qualified Data.MultiSet              as MS
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

p6 :: Puzzle
p6 = Puzzle "6" inputParser (pure pt1) mempty

type Input = [(Int,Int)]

inputParser :: Parser Input
inputParser = coordinate `endBy` newline
  where
    coordinate = do
      x <- L.decimal
      string ", "
      y <- L.decimal
      pure (x,y)

pt1 :: Input -> Text
pt1 input = show
            . maximum
            . map snd
            . frequencies
            . map snd
            . removeBoundaryLabels
            $ labeledPoints
  where
    minX = minimum . fmap fst $ input
    maxX = maximum . fmap fst $ input
    minY = minimum . fmap snd $ input
    maxY = maximum . fmap snd $ input
    labeledInput = zip ['a'..] input
    distancesAndCoords :: [(Coord, Distances)]
    distancesAndCoords = do
      x <- [minX..maxX]
      y <- [minY..maxY]
      pure ( (x,y)
           , fmap (second $ manhattan (x,y)) labeledInput
           )
    labeledPoints :: [(Coord, Char)]
    labeledPoints = catMaybes
                    . fmap sequence -- sequence (x, Just y) => Just (x,y); sequence (x, Nothing) => Nothing
                    . fmap ( second $ \dists ->
                               case sortOn snd dists of
                                 ((lbl,d1):(_,d2):_) | d1 == d2  -> Nothing
                                                     | otherwise -> Just lbl
                           )
                    $ distancesAndCoords
    boundaryLabels :: [Char]
    boundaryLabels = hashNub
                     . fmap snd
                     . filter (\((x,y),_) ->
                                 x == minX || x == maxX ||
                                 y == minY || y == maxY
                              )
                     $ labeledPoints
    removeBoundaryLabels = filter (not . (`elem` boundaryLabels) . snd)

type Coord = (Int,Int)
type Distances = [(Char,Int)]

manhattan :: Coord -> Coord -> Int
manhattan (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

frequencies :: Ord a => [a] -> [(a,Int)]
frequencies = MS.toOccurList . MS.fromList





exampleText = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
exampleInput = fromMaybe [] . parseMaybe inputParser $ exampleText
exampleOutput1 = pt1 exampleInput
