{-# LANGUAGE FlexibleContexts #-}
module AoC2018.P7 (p7) where

import           AoC2018
import           Universum
import           Universum.Unsafe            as Unsafe

import           Control.Monad.Writer.Strict
import qualified Data.HashMap.Strict         as HM
import           Text.Megaparsec
import           Text.Megaparsec.Char

p7 :: Puzzle
p7 = Puzzle "7" inputParser (pure pt1) mempty

type Input = [Dependency]
type Dependency = (Char,Char)

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



exampleText :: Text
exampleText = "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.\n"
exampleInput = fromMaybe [] . parseMaybe inputParser $ exampleText
exampleOutput1 = pt1 exampleInput
