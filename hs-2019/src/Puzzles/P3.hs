module Puzzles.P3 where

import Parse
import Puzzle

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

p3 :: Puzzle
p3 = Puzzle "3" inputParser pt1 pt2

data Direction = L | R | U | D
               deriving (Eq,Ord,Show)

data Directive = Directive Direction Int
                 deriving Show

type Loc = (Int,Int)

type Input = ([Directive], [Directive])

inputParser :: Parser Input
inputParser = (,) <$> directiveList <* newline <*> directiveList
  where
    directiveList = directive `sepBy` ","
    directive = Directive <$> direction <*> signedInteger
    direction = choice [ "L" >> pure L
                       , "R" >> pure R
                       , "U" >> pure U
                       , "D" >> pure D
                       ] :: Parser Direction

pt1 = Just
      . minimum
      . fmap manhattan
      . uncurry intersections
      . bimap enumeratePath enumeratePath

pt2 = Just
      . minimum
      . Map.elems
      . intersections'
      . bimap enumeratePath' enumeratePath'

enumeratePath = go (0 :: Int,0) D 0
  where
    go loc _ 0 [] = [loc]
    go loc _ 0 (Directive dir dist:rest) = go loc dir dist rest
    go loc dir dist rest = let update = case dir of
                                          L -> _1 %~ subtract 1
                                          R -> _1 %~ (+ 1)
                                          U -> _2 %~ (+ 1)
                                          D -> _2 %~ subtract 1
                           in loc : go (update loc) dir (dist - 1) rest

intersections path = filter (flip Set.member . Set.delete (0,0) . Set.fromList $ path)

manhattan (x,y) = abs x + abs y

enumeratePath' = go ((0,0),0 :: Int) D 0
  where
    go loc _ 0 [] = [loc]
    go loc _ 0 (Directive dir dist:rest) = go loc dir dist rest
    go loc dir dist rest = let update = case dir of
                                          L -> _1 . _1 %~ subtract 1
                                          R -> _1 . _1 %~ (+ 1)
                                          U -> _1 . _2 %~ (+ 1)
                                          D -> _1 . _2 %~ subtract 1
                           in loc : go (update loc & _2 %~ (+ 1)) dir (dist - 1) rest

intersections' = uncurry (Map.intersectionWith (+)) . bimap makeTable makeTable
  where
    makeTable = Map.delete (0,0) . Map.fromListWith min
