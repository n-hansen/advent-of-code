{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Puzzles.P8 where

import AocPrelude hiding (some)
import Parse
import Puzzle

import Optics.TH
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.RawString.QQ

type Pattern = Set Char

data InputLine = IL { _signals :: Set Pattern
                    , _output :: [Pattern]
                    } deriving (Show)

makeLenses ''InputLine

p8 :: Puzzle
p8 = Puzzle "8" inputParser pt1 pt2

inputParser :: Parser [InputLine]
inputParser = many il
  where
    pat = Set.fromList <$> some lowerChar <* actualSpaces
    il =
      IL . Set.fromList
      <$> some pat
      <* string "| "
      <*> some pat
      <* newline

digitPatterns :: [(Int,Pattern)]
digitPatterns =
  [(0,"abcefg")
  ,(1,"cf")
  ,(2,"acdeg")
  ,(3,"acdfg")
  ,(4,"bcdf")
  ,(5,"abdfg")
  ,(6,"abdefg")
  ,(7,"acf")
  ,(8,"abcdefg")
  ,(9,"abcdfg")
  ]
  & traversed % _2 %~ Set.fromList

digitSegmentCounts =
  digitPatterns
  & traversed % _2 %~ Set.size

digitsWithUniqueCounts =
  digitSegmentCounts
  & sortOn snd
  & groupBy ((==) `on` snd)
  & filter ((1 ==) . length)
  & fmap unsafeHead

pt1 = Just
  . length
  . filter (`elem` (fmap snd digitsWithUniqueCounts))
  . toListOf ( traversed
               % output
               % traversed
               % to length
             )

translations =
  ['a'..'g']
  & permutations
  & fmap (\scrambled ->
            let applyScramble = zip ['a'..'g'] scrambled
                                & Map.fromAscList
                                & (Map.!)
                scrambledPatterns = digitPatterns
                                    & traversed % _2 %~ Set.map applyScramble
            in ( Set.fromList
                 . fmap snd
                 $ scrambledPatterns
               , Map.fromList
                 . fmap swap
                 $ scrambledPatterns
               )
         )
  & Map.fromList

pt2 =
  Just
  . sum
  . fmap decode
  where
    decode IL{_signals,_output} =
      let digitMap = translations Map.! _signals
          digits = fmap (digitMap Map.!) $ _output
          number = sum . zipWith (\e d -> 10^e * d) [0..] . reverse $ digits
      in number
