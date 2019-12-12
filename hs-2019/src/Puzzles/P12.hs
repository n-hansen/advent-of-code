{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P12 where

import Parse
import Puzzle

type Vector = (Int,Int,Int)

data Planet = P { _position :: Vector
                , _velocity :: Vector
                } deriving (Eq,Show)
makeLenses ''Planet

type Input = [Planet]

inputParser :: Parser Input
inputParser = planet `endBy` newline <* optional newline
  where
    planet = do
      "<x="
      x <- signedInteger
      ", y="
      y <- signedInteger
      ", z="
      z <- signedInteger
      ">"
      pure $ P (x,y,z) (0,0,0)

applyTriples f (x1,y1,z1) (x2,y2,z2) = (f x1 x2, f y1 y2, f z1 z2)

step :: [Planet] -> [Planet]
step planets = planets
               & applyAllGravity
               & move
  where
    applyAllGravity = fmap $ \p -> foldl' applyPlanetGravity p planets
    applyPlanetGravity :: Planet -> Planet -> Planet
    applyPlanetGravity me them = let deltaV = applyTriples (-) (them ^. position) (me ^. position)
                                              & each %~ signum
                                 in me & velocity %~ applyTriples (+) deltaV
    move = fmap $ \p -> p & position %~ applyTriples (+) (p ^. velocity)

energy :: [Planet] -> Int
energy = sum . fmap (liftA2 (*) kinetic potential)
  where
    kinetic = views position e
    potential = views velocity e
    e = sum . fmap abs . toListOf each

pt1 = Just . energy . unsafeHead . drop 1000 . iterate step

pt2 _ = Nothing :: Maybe ()


p12 :: Puzzle
p12 = Puzzle "12" inputParser pt1 pt2
