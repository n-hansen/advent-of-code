module Puzzles.P5 where

import AocPrelude
import qualified Data.Map.Strict as Map
import Data.List (tail)
import Parse
import Puzzle

p5 :: Puzzle
p5 = Puzzle "5" inputParser1 inputParser2 pt1 pt2

type Input1 = (Map Int [Crate], Instructions)
type Input2 = Input1

type CrateConfiguration = [[Crate]]
type Crate = Char
type Instructions = [(Int,Int,Int)]

inputParser1 :: Parser Input1
inputParser1 =
  (,)
  <$> crateConfig
  <* many newline
  <*> instructions
  where
    stackRow = stackSpot `sepBy1` string " "
    stackSpot :: Parser (Maybe Crate)
    stackSpot = Just <$> crate <|> Nothing <$ string "   "
    crate = do
      void $ string "["
      c <- satisfy isAlpha
      void $ string "]"
      pure c
    stackLabel = do
      void $ string " "
      n <- unsignedInteger
      void . optional $ string " "
      pure n
    crateConfig = do
      stacks <- fmap catMaybes . transpose <$> stackRow `endBy1` newline
      stackLabels <- stackLabel `sepBy1` string " "
      guard $ length stacks == length stackLabels
      pure . Map.fromList $ zip stackLabels stacks
    instruction = do
      void $ string "move "
      count <- unsignedInteger
      void $ string " from "
      fromStack <- unsignedInteger
      void $ string " to "
      toStack <- unsignedInteger
      pure (count,fromStack,toStack)
    instructions = instruction `endBy1` newline


inputParser2 :: Parser Input2
inputParser2 = inputParser1

runInstructions :: Input1 -> Map Int [Crate]
runInstructions (crates, instructions) = execState (go instructions) crates
  where
    go :: Instructions -> State (Map Int [Crate]) ()
    go [] = pure ()
    go ((1,f,t):rest) = do
      c <- pickUp f
      drop c t
      go rest
    go ((n,f,t):rest) = do
      c <- pickUp f
      drop c t
      go ((n-1,f,t):rest)
    pickUp :: Int -> State (Map Int [Crate]) Crate
    pickUp col = do
      c <- preuse $ ix col . _head
      ix col %= tail
      case c of
        Just c -> pure c
        _ -> panic "pulled off an empty stack"
    drop c col =
      ix col %= (c:)

topCrates = fmap (unsafeHead . snd) . Map.toAscList

pt1 = Just . topCrates . runInstructions

runInstructions2 :: Input1 -> Map Int [Crate]
runInstructions2 (crates, instructions) = execState (go instructions) crates
  where
    go :: Instructions -> State (Map Int [Crate]) ()
    go [] = pure ()
    go ((n,f,t):rest) = do
      cs <- use $ ix f . to (take n)
      ix f %= drop n
      ix t %= (cs <>)
      go rest

pt2 = Just . topCrates . runInstructions2
