module Puzzles.P14 where

import Parse
import Puzzle

import Data.Char
import Data.List (partition)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable

p14 :: Puzzle
p14 = Puzzle "14" inputParser pt1 pt2

newtype Chemical = C Text deriving (Show,Eq,Hashable,IsString)

data Equation = Eqn (Chemical, Integer) [(Chemical, Integer)]
              deriving Show

type ReactionTable = HashMap Chemical Equation

type Input = ReactionTable

inputParser :: Parser Input
inputParser = HashMap.fromList <$> equation `endBy` newline
  where
    equation = do
      components <- chemicalCount `sepBy` ", "
      " => "
      result@(r,_) <- chemicalCount
      pure $ (r, Eqn result components)
    chemicalCount = do
      count <- fromIntegral <$> unsignedInteger
      space
      name <- takeWhileP (Just "chemical name") isAlpha
      pure (C name, count)

data Reaction = Rxn [(Chemical,Integer)] (HashMap Chemical Integer)
              deriving Show

computeOreFor reactionTable thing = go 0 HashMap.empty [thing]
  where
    go ore scrap [] = ore
    go ore scrap ((c, need):queued) =
      let lyingAround = HashMap.lookupDefault 0 c scrap
          Just (Eqn (_, willGet) cost) = HashMap.lookup c reactionTable
          divMod'@(_,someLeftovers) = (need - lyingAround) `divMod` willGet
          (howManyTimes,leftovers) = if someLeftovers == 0
                                     then divMod'
                                     else bimap (1 +) (willGet -) divMod'
          (oreCost, otherCost) = first (sum . fmap snd)
                                 . partition (("ORE" ==) . fst)
                                 . fmap (second (* howManyTimes))
                                 $ cost
          scrap' = HashMap.insert c leftovers scrap
      in if lyingAround >= need
         then go ore (HashMap.update (pure . subtract need) c scrap) queued
         else go (ore+oreCost) scrap' (otherCost <> queued)

pt1 input = Just $ computeOreFor input ("FUEL",1)

computeFuelFrom rxnTbl = binSearch 0 searchStart
  where
    oreStore = 1_000_000_000_000 :: Integer
    minOrePerFuel = computeOreFor rxnTbl ("FUEL",1)
    searchStart = 2 * oreStore `div` minOrePerFuel
    binSearch lo hi
      | lo + 1 >= hi = lo
      | otherwise = let mid = (lo + hi) `div` 2
                        midOre = computeOreFor rxnTbl ("FUEL", mid)
                    in case compare midOre oreStore of
                         LT -> binSearch mid hi
                         GT -> binSearch lo mid
                         EQ -> mid

pt2 = Just . computeFuelFrom
