{-# LANGUAGE FlexibleContexts #-}
module AoC2018.P7 (p7) where

import           AoC2018
import           Universum
import           Universum.Unsafe            as Unsafe

import           Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict         as HM
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS
import           Data.List                   (partition)
import           Text.Megaparsec
import           Text.Megaparsec.Char

p7 :: Puzzle
p7 = Puzzle "7" inputParser (pure pt1) (pure pt2)

type Input = [Dependency]
type Step = Char
type Dependency = (Step,Step)

inputParser :: Parser Input
inputParser = dependency `endBy` newline
  where
    dependency = do
      string "Step "
      before <- letterChar
      string " must be finished before step "
      after <- letterChar
      string " can begin."
      pure (before, after)

pt1 :: Input -> Text
pt1 = toText . execWriter . topoTraverse
  where
    topoTraverse deps = do
      let remainingSteps = [x | (a,b) <- deps, x <- [a,b]]
          stepDeps :: HM.HashMap Char Int
          stepDeps = foldl'
                     (\acc (_,s) -> HM.adjust (+ 1) s acc)
                     ( foldl'
                       (\acc s -> HM.insert s 0 acc)
                       mempty
                       remainingSteps
                     )
                     deps
          nextStep = snd
                     . Unsafe.head
                     . sort
                     . fmap (\(a,b)->(b,a))
                     . HM.toList
                     $ stepDeps
          remainingDeps = filter (\(s,_) -> s /= nextStep) deps
          finalSteps = sort . filter (/= nextStep) $ remainingSteps
      tell [nextStep]
      if (null remainingDeps)
        then tell finalSteps
        else topoTraverse remainingDeps

pt2 :: Input -> Text
pt2 deps = show . go 0 mempty allSteps $ HS.fromList deps
  where
    workers = 5
    stepLength s = fromEnum s - fromEnum 'A' + 61
    allSteps = HS.fromList [x | (a,b) <- deps, x <- [a,b]]
    go :: Int -> HashSet (Step,Int) -> HashSet Step -> HashSet Dependency -> Int
    go t inProgress remainingSteps activeDeps =
      if null remainingSteps
      then t
      else
        let
          blocked = HS.map snd activeDeps
          freeSteps = sort
                      . toList
                      . HS.difference remainingSteps
                      . HS.union blocked
                      $ HS.map fst inProgress
          freeWorkers = workers - length inProgress
          newSteps = HS.fromList
                     . fmap (\s -> (s, stepLength s))
                     . take freeWorkers
                     $ freeSteps
          inProgress' = inProgress <> newSteps
          wait = minimum . HS.map snd $ inProgress'
          completed = HS.filter ((0 ==) . snd)
                      . HS.map (second (subtract wait))
                      $ inProgress'
          inProgress'' = HS.filter ((0 /=) . snd)
                         . HS.map (second (subtract wait))
                         $ inProgress'
          isComplete s = s `elem` HS.map fst completed
          remainingSteps' = HS.filter (not . isComplete) remainingSteps
          activeDeps' = HS.filter (not . isComplete . fst) activeDeps
        in
          go (t + wait) inProgress'' remainingSteps' activeDeps'


exampleText :: Text
exampleText = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n"
exampleInput = fromMaybe [] . parseMaybe inputParser $ exampleText
exampleOutput1 = pt1 exampleInput
exampleOutput2 = pt2 exampleInput
