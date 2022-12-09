module Puzzles.P6 where

import AocPrelude
import Data.List.Extra (nubOrd)
import qualified Data.Sequence as S
import qualified Data.Text as T
import Parse
import Puzzle

p6 :: Puzzle
p6 = Puzzle "6" inputParser1 inputParser2 pt1 pt2

type Input1 = [Char]
type Input2 = Input1

inputParser1 :: Parser Input1
inputParser1 = T.unpack <$> takeWhile1P Nothing isAlpha <* optional newline

inputParser2 :: Parser Input2
inputParser2 = inputParser1

findStartIndex = go (0 :: Int)
  where
    go offset (a:b:c:d:rest)
      | length (nubOrd [a,b,c,d]) == 4 = offset + 4
      | otherwise = go (offset+1) (b:c:d:rest)
    go _ _ = panic "couldn't find start"

pt1 = Just . findStartIndex

findStartOfMessage input = go windowSize initWindow initTail
  where
    windowSize = 14 :: Int
    (initWindow,initTail) = first S.fromList . splitAt windowSize $ input
    go offset window tail
      | allDistinct window = offset
    go offset (_ :< window) (x:xs) = go (offset+1) (window |> x) xs
    go _ _ _ = panic "couldn't find start of message"
    allDistinct xs = length xs == (length . nubOrd . toList $ xs)

pt2 = Just . findStartOfMessage
