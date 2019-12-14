{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P13 where

import Parse
import Puzzle

import IntcodeComputer

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Input = Tape

inputParser :: Parser Input
inputParser = tapeParser

data TileId = Blank | Wall | Block | Paddle | Ball
            deriving (Eq,Show)

data Tile = Tile TileId (Int,Int)
          | Score Int
          deriving (Show)

interpretOutput (x:0:s:rest) | x == -1 = Score s : interpretOutput rest
interpretOutput (x:y:tid:rest) = let t = case tid of
                                              0 -> Blank
                                              1 -> Wall
                                              2 -> Block
                                              3 -> Paddle
                                              4 -> Ball
                                    in Tile t (x,y) : interpretOutput rest
interpretOutput _ = []

pt1 input = Just $
  case initProgram input & runProgram
  of
    Halted (Output o) -> interpretOutput o
                         & filter (\case
                                      Tile Block _ -> True
                                      _ -> False
                                  )
                         & length

{-

data GameState = GS { _screen :: Map (Int,Int) TileId
                    , _score :: Int
                    } deriving Show
makeLenses ''GameState

instance Pretty GameState where
  pretty = viaShow

updateGamestate = foldl' $
                  \gs t -> case t of
                             Tile t loc -> gs & screen . at loc .~ Just t
                             Score s -> gs & score .~ s


runAgent = go 0
  where
    go nextInput st = case st & provideInput [nextInput] & runProgram of
                        WaitingForInput (DrainedOutput o st') ->
                          case computeGameState . interpretOutput $ o of
                            GS _ _ _ (Just 999) -> undefined
                            gs@(GS _ _ 0 (Just s)) -> show $ st' & provideInput [0..] & runProgram :: Text
                            GS (Just b) (Just p) x _ ->
                              traceShow x $
                              if | b < p -> go (-1) st'
                                 | b > p -> go 1 st'
                                 | otherwise -> go 0 st'

addQuarter = ix 0 .~ 2

-}

pt2 _ = Nothing :: Maybe () --  Just . runAgent . initProgram . addQuarter


p13 :: Puzzle
p13 = Puzzle "13" inputParser pt1 pt2
