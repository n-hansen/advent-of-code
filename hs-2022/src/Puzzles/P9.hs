module Puzzles.P9 where

import AocPrelude
import Parse
import Puzzle

import Control.Monad.Trans.Maybe
import Control.Monad.RWS.Strict
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type Instruction = (Direction,Int)
data Direction = U | D | L | R deriving (Eq,Show)
type Coord = (Int,Int)

p9 :: Puzzle
p9 = Puzzle "9" inputParser1 inputParser2 pt1 pt2

type Input1 = [Instruction]
type Input2 = Input1

inputParser1 :: Parser Input1
inputParser1 = instruction `endBy1` newline
  where
    instruction = (,) <$> direction <* string " " <*> unsignedInteger
    direction = choice [ string "D" $> D
                       , string "U" $> U
                       , string "L" $> L
                       , string "R" $> R
                       ]

inputParser2 :: Parser Input2
inputParser2 = inputParser1

headLoc = _1
tailLoc = _2

step :: Direction -> RWS () (Set Coord) (Seq Coord) ()
step dir = do
  ropeLength <- gets Seq.length

  -- move head
  case dir of
    U -> ix 0 . _2 %= (+ 1)
    D -> ix 0 . _2 %= subtract 1
    L -> ix 0 . _1 %= (+ 1)
    R -> ix 0 . _1 %= subtract 1

  let moveKnot :: Int -> MaybeT (RWS () (Set Coord) (Seq Coord)) ()
      moveKnot n = when (n < ropeLength) $ do
        Just (x0,y0) <- preuse $ ix (n - 1)
        Just (x1,y1) <- preuse $ ix n
        let dx = x0 - x1
            dy = y0 - y1
        when (abs dx >= 2 || abs dy >= 2) $ do
          ix n %= bimap (+ signum dx) (+ signum dy)
          moveKnot $ n + 1
  runMaybeT $ moveKnot 1
  use (ix (ropeLength - 1) . to Set.singleton) >>= tell

run :: Int -> [Instruction] -> Set Coord
run ropeLength prgm = snd $ execRWS (tell (Set.singleton startLoc) >> go prgm) () initRope
  where
    startLoc = (0,0)
    initRope = Seq.replicate ropeLength startLoc
    go [] = pure ()
    go ((_,0):rest) = go rest
    go ((d,n):rest) = step d >> go ((d,n-1):rest)


pt1 = Just . length . run 2

pt2 = Just . length . run 10
