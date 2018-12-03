{-# LANGUAGE NamedFieldPuns #-}
module AoC2018.P3 where

import Universum

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.HashSet (member,insert,size)

data Claim = Claim { claimId :: Int
                   , hOffset :: Int
                   , vOffset :: Int
                   , width   :: Int
                   , height  :: Int
                   } deriving (Show)

type Input = [Claim]

type Parser = Parsec Void Text

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

parseInput :: Text -> Input
parseInput input =
  case parse inputParser "" input of
    Left bundle -> error . toText $ errorBundlePretty bundle
    Right p -> p

runPt1 :: Text -> IO ()
runPt1 = print . size . findOverlaps . parseInput
  where
    findOverlaps =
      snd
      . foldl' (\(seen,overlaps) new ->
                  ( insert new seen
                  , if new `member` seen then insert new overlaps else overlaps
                  )
               ) (mempty, mempty)
      . enumeratePoints
    enumeratePoints claims = do
      Claim{hOffset,vOffset,width,height} <- claims
      x <- [hOffset..hOffset+width-1]
      y <- [vOffset..vOffset+height-1]
      pure (x,y)

