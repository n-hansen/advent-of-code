{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P13 where

import AocPrelude
import Parse
import Puzzle

import Optics.TH
import Prettyprinter

data Input = Input { _dots :: [(Int,Int)]
                   , _creases :: [Crease]
                   } deriving (Show)
data Crease = AlongX Int
            | AlongY Int
            deriving (Show)
makeLenses ''Input
makePrisms ''Crease

p13 :: Puzzle
p13 = Puzzle "13" inputParser pt1 pt2

inputParser :: Parser Input
inputParser = Input <$> dot `endBy` newline <* newline <*> crease `endBy` newline
  where
    dot = (,) <$> unsignedInteger <* string "," <*> unsignedInteger
    crease = do
      _ <- string "fold along "
      dir <- (AlongY <$ string "y=") <|> (AlongX <$ string "x=")
      dir <$> unsignedInteger

makeCreases input =
  case input ^. creases of
    c:cs -> input
            & applyCrease c
            & creases .~ cs
            & makeCreases
    [] -> input ^. dots

applyCrease c = dots % each % coord %~ \w -> if w <= line then w else 2*line-w
  where
    (coord, line) = case c of
                      AlongX xline -> (_1, xline)
                      AlongY yline -> (_2, yline)


pt1 =
  Just
  . length
  . ordNub
  . makeCreases
  . (creases %~ take 1)

newtype Display = Display [(Int,Int)] deriving (Eq,Show)

instance Pretty Display where
  pretty (Display ds) =
    let xMax = maximum $ fmap fst ds
        yMax = maximum $ fmap snd ds
        grid = [ [ if (x,y) `elem` ds then "#" else " "
                 | x <- [0..xMax]
                 ]
               | y <- [0..yMax]
               ]
        renderRow = foldr (<>) hardline
    in foldr ((<>) . renderRow) mempty grid

pt2 = Just
  . Display
  . ordNub
  . makeCreases
