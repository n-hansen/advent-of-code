module AoC2018.P2 (p2) where

import           AoC2018
import           Universum            hiding (elems, many)

import           Data.HashMap.Strict  (elems, insertWith)
import           Text.Megaparsec
import           Text.Megaparsec.Char

p2 :: Puzzle
p2 = Puzzle "2" inputParser (pure pt1) (pure pt2)

type Input = [String]

inputParser :: Parser Input
inputParser = many letterChar `endBy` newline

pt1 :: Input -> Text
pt1 = show . computeChecksum . fmap countLetterReps
  where
    countLetterReps = foldl' (\repMap ltr -> insertWith (+) ltr 1 repMap) mempty
    computeChecksum repMaps = countReps 2 repMaps * countReps 3 repMaps
    countReps n = length . filter (not . null . filter (== n) . elems)

pt2 :: Input -> Text
pt2 = formatAnswer . computeAnswer
  where
    computeAnswer :: [String] -> (String, String)
    computeAnswer [id1, id2] = (id1, id2)
    computeAnswer (thisId:remainingIds) =
      case filter (\otherId -> charDiff thisId otherId == 1) remainingIds of
        []          -> computeAnswer remainingIds
        (otherId:_) -> (thisId, otherId)

    charDiff :: String -> String -> Int
    charDiff id1 id2 = length . filter id $ zipWith (/=) id1 id2

    formatAnswer :: (String, String) -> Text
    formatAnswer (id1,id2) = unlines [ toText id1
                                     , toText id2
                                     , toText . map fst . filter (uncurry (==)) $ zip id1 id2
                                     ]
