module Puzzles.P4 where

import Parse
import Puzzle
import Util

p4 :: Puzzle
p4 = Puzzle "4" inputParser pt1 pt2

type Input = [Int]

inputParser :: Parser Input
inputParser = enumFromTo <$> unsignedInteger <* "-" <*> signedInteger


digits = fmap reverse . unfoldr $ \number -> if number > 0 then Just (number `mod` 10, number `div` 10) else Nothing

increasing = all (uncurry (<=)) . window2


pt1 = Just . length . filter pt1Criteria

pt1Criteria = liftM2 (&&) increasing hasDouble
              . group
              . digits
  where
    hasDouble = any ((2 <=) . length)

pt2 = Just . length . filter pt2Criteria

pt2Criteria = liftM2 (&&) increasing hasDouble
              . group
              . digits
  where
    hasDouble = any ((2 ==) . length)
