module Puzzles.P12 where

import AocPrelude
import Parse
import Puzzle

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

p12 :: Puzzle
p12 = Puzzle "12" inputParser pt1 pt2

type Topology = Map Cave [Cave]

data Cave = Big Text | Small Text deriving (Eq,Show,Ord)


inputParser :: Parser Topology
inputParser = Map.unionsWith (<>) <$> connection `endBy` newline
  where
    connection :: Parser Topology
    connection = do
      begin <- cave
      _ <- string "-"
      end <- cave
      pure $ Map.fromList [(begin, [end])
                          ,(end, [begin])
                          ]
    cave = bigCave <|> smallCave
    bigCave = Big <$> takeWhile1P (Just "big cave") isUpper
    smallCave = Small <$> takeWhile1P (Just "small cave") isLower

countRoutes :: Topology -> Int
countRoutes topology = length $ go (Set.singleton start) start
  where
    start = Small "start"
    end = Small "end"
    go seen current = do
      exit <- topology Map.! current
              & filter (flip Set.notMember seen)
      if exit == end then pure ()
        else let seen' = case exit of
                           Big _ -> seen
                           Small _ -> Set.insert exit seen
             in go seen' exit


pt1 = Just . countRoutes

countRoutes2 :: Topology -> Int
countRoutes2 topology = length $ go (Set.singleton start) start False
  where
    start = Small "start"
    end = Small "end"
    go seen current returnedToSmallCave = do
      exit <- topology Map.! current
              & filter (/= start)
      let revisiting = exit `Set.member` seen
      case exit of
        Small _ | exit == end -> pure ()
                | revisiting && returnedToSmallCave -> mzero
                | revisiting -> go seen exit True
                | otherwise -> go (Set.insert exit seen) exit returnedToSmallCave
        Big _ -> go seen exit returnedToSmallCave

pt2 = Just . countRoutes2
