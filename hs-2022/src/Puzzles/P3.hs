module Puzzles.P3 where

import AocPrelude
import Parse
import Puzzle
import Util
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

p3 :: Puzzle
p3 = Puzzle "3" inputParser1 inputParser2 pt1 pt2

type Input1 = [Rucksack]
type Input2 = Input1

type Item = Char
type Rucksack = [Char]

inputParser1 :: Parser Input1
inputParser1 = (T.unpack <$> takeWhile1P Nothing (/= '\n')) `endBy` newline

inputParser2 :: Parser Input2
inputParser2 = inputParser1

compartmentalize :: Rucksack -> ([Item],[Item])
compartmentalize r = splitAt (length r `div` 2) r

priority :: Item -> Int
priority = (Map.!) . Map.fromList $ zip (['a'..'z'] <> ['A'..'Z']) [1..52]

pt1 = Just . sum . fmap (priority . uncurry findDupe . compartmentalize)

findDupe l = unsafeHead . filter (`elem` l)

pt2 = Just . sum . fmap (priority . findDupe3) . partition3

partition3 (a:b:c:rest) = (a,b,c):partition3 rest
partition3 _ = []

findDupe3 (a,b,c) = unsafeHead . filter (`elem` ab) $ c
  where
    ab = filter (`elem` a) b
