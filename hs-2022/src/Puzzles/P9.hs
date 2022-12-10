module Puzzles.P9 where

import AocPrelude
import Parse
import Puzzle

import Control.Monad.RWS.Strict
import qualified Data.Set as Set

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

step :: Direction -> RWS () (Set Coord) (Coord,Coord) ()
step dir = do
  -- move head
  case dir of
    U -> headLoc . _2 %= (+ 1)
    D -> headLoc . _2 %= subtract 1
    L -> headLoc . _1 %= (+ 1)
    R -> headLoc . _1 %= subtract 1

  -- move tail
  (hx,hy) <- use headLoc
  (tx,ty) <- use tailLoc
  let dx = hx - tx
      dy = hy - ty
  when (abs dx >= 2 || abs dy >= 2) $ do
    tailLoc . _1 %= (+ signum dx)
    tailLoc . _2 %= (+ signum dy)
    use (tailLoc . to Set.singleton) >>= tell

run :: [Instruction] -> Set Coord
run prgm = snd $ execRWS (tell (Set.singleton startLoc) >> go prgm) () (startLoc,startLoc)
  where
    startLoc = (0,0)
    go [] = pure ()
    go ((_,0):rest) = go rest
    go ((d,n):rest) = step d >> go ((d,n-1):rest)


pt1 = Just . length . run

pt2 _ = Nothing :: Maybe ()
