{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
module AoC2018.P6 (p6) where

import           AoC2018
import           Universum

import           Data.Array                 (Array)
import qualified Data.Array                 as A
import           Data.Hashable
import qualified Data.HashMap.Strict        as HM
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

data Label = Unlabeled
           | SingleLabel Char
           | MultipleLabels
           deriving (Show,Eq,Generic,Hashable)

instance Container (Array i e) -- this is some bullshit

pt1 :: Input -> Text
pt1 input = show $ findLargestArea labeledGrid
  where
    minX = minimum . fmap fst $ input
    maxX = maximum . fmap fst $ input
    minY = minimum . fmap snd $ input
    maxY = maximum . fmap snd $ input
    minBd = (minX,minY)
    maxBd = (maxX,maxY)
    gridBds = (minBd,maxBd)
    initialGrid = A.accumArray
                  (const id)
                  Unlabeled
                  gridBds
                  . zip input
                  $ SingleLabel <$> ['a'..]
    labeledGrid = computeAreas initialGrid
    computeAreas grid =
      let
        neighborhood (x,y) = do
          x' <- [x-1,x,x+1]
          y' <- [y-1,y,y+1]
          guard $ x == x' || y == y'
          guard $
            minX <= x' && x' <= maxX
            && minY <= y' && y' <= maxY
          pure (x',y')
        label loc =
          case grid A.! loc of
            Unlabeled -> Just . (,) loc $
              case filter (/= Unlabeled) . fmap (grid A.!) $ neighborhood loc of
                []                          -> Unlabeled
                (sl@(SingleLabel lbl):lbls) -> if all (== sl) lbls
                                               then sl
                                               else MultipleLabels
                _                           -> MultipleLabels
            _ -> Nothing
      in
        if all (/= Unlabeled) grid
        then grid
        else computeAreas
             . (A.//) grid
             . catMaybes
             . fmap label
             . A.indices
             $ grid
    findLargestArea = maximum . HM.elems . removeBoundaries . measureAreas
    measureAreas = foldl'
                   (\counts lbl ->
                       case lbl of
                         SingleLabel _ -> HM.alter (pure . maybe 1 (+ 1)) lbl counts
                         MultipleLabels -> counts
                   )
                   mempty
    removeBoundaries = HM.filterWithKey (\k _ -> not $ k `elem` boundaryLabels)
    boundaryLabels = hashNub
                     . fmap (labeledGrid A.!)
                     $ [(x,y) | x <- [minX..maxX]
                              , y <- [minY..maxY]
                              , x == minX || x == maxX
                                || y == minY || y == maxY]
    -- pprintGrid =
    --   toText . concat .
    --   fmap (\((x,y),lbl) ->
    --           (if y == 1 then "\n" else ""
    --           ) <>
    --           case lbl of
    --             SingleLabel lbl -> [lbl]
    --             _               -> "."
    --           ) . A.assocs



exampleText = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9\n"
exampleInput = fromMaybe [] . parseMaybe inputParser $ exampleText
exampleOutput = pt1 exampleInput
