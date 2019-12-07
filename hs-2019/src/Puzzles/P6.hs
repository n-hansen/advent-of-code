{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P6 where

import Parse
import Puzzle

import Data.Char
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List hiding (sum)

newtype Planet = Planet { unwrapPlanet :: Text } deriving (Eq,Ord,Show)

data OrbitRel = OR Planet Planet
              deriving (Show,Eq)

data OrbitGraph = OG Planet [OrbitGraph]
                deriving (Show)
makeBaseFunctor ''OrbitGraph

p6 :: Puzzle
p6 = Puzzle "6" inputParser pt1 pt2

type Input = OrbitGraph

inputParser :: Parser Input
inputParser = buildGraph <$> orbitRel `endBy` newline
  where
    orbitRel = OR <$> planet <* string ")" <*> planet
    planet = Planet <$> takeWhile1P (Just "planet") isAlphaNum

buildGraph :: [OrbitRel] -> OrbitGraph
buildGraph = ana coalg . (Planet "COM",)
  where
    coalg (p, remaining) =
      let (children, rest) = partition (\(OR parent _) -> parent == p) remaining
      in OGF p (fmap (\(OR _ child) -> (child, rest)) children)

countOrbits :: OrbitGraph -> Int
countOrbits = fst . cata alg
  where
    alg (OGF _ children) = let descendantCount = sum . fmap snd $ children
                               orbitCount = sum . fmap fst $ children
                           in (orbitCount + descendantCount + length children, descendantCount + length children)

pt1 = Just . countOrbits

data OrbitalData = HaveAnswer Int
                 | SantaDist Int
                 | YouDist Int
                 | NoData
                 deriving Show

{-
countTransfers :: OrbitGraph -> Int
countTransfers = preview _Left . cata alg
  where
    alg (OGF p children) =
      if | p == santa -> Just $ Left 0
         | p == you   -> Just $ Right 0
         | otherwise  -> case bimap minimumMay minimumMay
                              . partitionEithers
                              . catMaybes
                              $ children
                         of (Nothing,Nothing) -> Nothing
                            (Just s, Just y)  -> 1 + s + y
                            (Just s, Nothing) -> 1 + s
                            (Nothing, Just y) -> 1 + y

--}

pt2 _ = Nothing :: Maybe ()
