{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P14 where

import AocPrelude
import Parse
import Puzzle
import Util


import qualified Data.Map.Strict as Map

p14 :: Puzzle
p14 = Puzzle "14" inputParser pt1 pt2

data Input = Input { seed :: [Char]
                   , rules :: Map (Char,Char) Char
                   } deriving (Show)

inputParser :: Parser Input
inputParser =
  Input
  <$> many letterChar
  <* newline <* newline
  <*> rulebook
  where
    rulebook = Map.fromList <$> rule `endBy` newline
    rule = do
      l <- letterChar
      r <- letterChar
      string " -> "
      o <- letterChar
      pure ((l,r),o)

runRules rules = go
  where
    go (a:b:cs) | Just generated <- Map.lookup (a,b) rules = a:generated:go (b:cs)
                | otherwise = a:go (b:cs)
    go str = str

pt1 Input{seed,rules} = Just $ mostCommon - leastCommon
  where
    (leastCommon :< (_ :> mostCommon)) =
      sort
      . fmap length
      . group
      . sort
      . unsafeHead
      . drop 10
      . iterate (runRules rules)
      $ seed

type Pair = (Char,Char)

pt2 Input{seed,rules} = Just $ mostCommon - leastCommon
  where
    pairRules :: Map Pair (Pair,Pair)
    pairRules = Map.mapWithKey (\(a,b) c -> ((a,c),(c,b))) rules

    step :: Map Pair Int -> Map Pair Int
    step counts =
      Map.foldrWithKey'
      (\pair count newCounts ->
         case Map.lookup pair pairRules of
           Just (p1,p2) -> newCounts
                           & Map.insertWith (+) p1 count
                           & Map.insertWith (+) p2 count
           Nothing -> newCounts
                      & Map.insertWith (+) pair count
      )
      Map.empty
      counts

    initialPairs =
      seed
      & window2
      & fmap (flip Map.singleton 1)
      & Map.unionsWith (+)

    (seedStart :< (_ :> seedEnd)) = seed

    pairCountsToLetterCounts pc =
      fmap (`div` 2)
      . adjustForInitialAndFinalLetters
      . sortOn snd
      . fmap (\ps@((p,_):_) -> (p, sum . fmap snd $ ps))
      . groupBy ((==) `on` fst)
      . sortOn fst
      $ (do ((a,b), cnt) <- Map.toList pc
            [(a,cnt),(b,cnt)]
        )

    adjustForInitialAndFinalLetters =
      fmap $ \(ltr,cnt) -> if ltr == seedStart || ltr == seedEnd then cnt+1 else cnt

    (leastCommon :< (_ :> mostCommon)) =
      initialPairs
      & iterate step
      & drop 40
      & unsafeHead
      & pairCountsToLetterCounts
