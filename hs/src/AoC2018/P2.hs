module AoC2018.P2 where

import Universum hiding (elems)

import Data.HashMap.Strict (insertWith,elems)

type Input = [String]

parseInput :: Text -> Input
parseInput = fmap toString . lines

runPt1 :: Text -> IO ()
runPt1 = print . computeChecksum . fmap countLetterReps . parseInput
  where
    countLetterReps = foldl' (\repMap ltr -> insertWith (+) ltr 1 repMap) mempty
    computeChecksum repMaps = countReps 2 repMaps * countReps 3 repMaps
    countReps n = length . filter (not . null . filter (== n) . elems)

runPt2 :: Text -> IO ()
runPt2 = putStrLn . formatAnswer . computeAnswer . parseInput
  where
    computeAnswer :: Input -> (String, String)
    computeAnswer [id1, id2] = (id1, id2)
    computeAnswer (thisId:remainingIds) =
      case filter (\otherId -> charDiff thisId otherId == 1) remainingIds of
        [] -> computeAnswer remainingIds
        (otherId:_) -> (thisId, otherId)

    charDiff :: String -> String -> Int
    charDiff id1 id2 = length . filter id $ zipWith (/=) id1 id2

    formatAnswer :: (String, String) -> Text
    formatAnswer (id1,id2) = unlines [ toText id1
                                     , toText id2
                                     , toText . map fst . filter (uncurry (==)) $ zip id1 id2
                                     ]
