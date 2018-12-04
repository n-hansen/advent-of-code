{-# LANGUAGE NamedFieldPuns #-}
module AoC2018.P3 (p3) where

import AoC2018
import Universum
import qualified Universum.Unsafe as Unsafe

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.HashSet (member,insert,size)

p3 :: Puzzle
p3 = Puzzle "3" inputParser (pure pt1) (pure pt2)

data Claim = Claim { claimId :: Int
                   , hOffset :: Int
                   , vOffset :: Int
                   , width   :: Int
                   , height  :: Int
                   } deriving (Show)

type Input = [Claim]

claimParser :: Parser Claim
claimParser = do
  Claim <$>
    (char '#' >> L.decimal) <*>
    (string " @ " >> L.decimal) <*>
    (char ',' >> L.decimal) <*>
    (string ": " >> L.decimal) <*>
    (char 'x' >> L.decimal)

inputParser :: Parser Input
inputParser = claimParser `endBy` newline

pt1 :: Input -> Text
pt1 = show . size . findOverlaps

findOverlaps :: [Claim] -> HashSet (Int,Int)
findOverlaps claims = snd
                      . foldl' (\(seen,overlaps) new ->
                                  ( insert new seen
                                  , if new `member` seen then insert new overlaps else overlaps
                                  )
                               ) (mempty, mempty)
                      $ enumeratePoints =<< claims

enumeratePoints :: Claim -> [(Int,Int)]
enumeratePoints Claim{hOffset,vOffset,width,height} = do
  x <- [hOffset..hOffset+width-1]
  y <- [vOffset..vOffset+height-1]
  pure (x,y)


pt2 :: Input -> Text
pt2 allClaims = show answer
  where
    overlaps = findOverlaps allClaims
    answer = claimId . Unsafe.head . filter checkClaim $ allClaims
    checkClaim = all (\pt -> not $ pt `member` overlaps) . enumeratePoints
