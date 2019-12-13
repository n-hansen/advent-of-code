{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Puzzles.P12 where

import Parse
import Puzzle

import Data.List.Extra hiding (sum)

type P3 = (Int,Int,Int)

data Planet space = P { _position :: space
                      , _velocity :: space
                      } deriving (Functor,Eq,Show)
makeLenses ''Planet

type Input = [Planet P3]

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

step :: [Planet P3] -> [Planet P3]
step planets = planets
               & applyAllGravity
               & move
  where
    applyAllGravity = fmap $ \p -> foldl' applyPlanetGravity p planets
    applyPlanetGravity :: Planet P3 -> Planet P3 -> Planet P3
    applyPlanetGravity me them = let deltaV = applyTriples (-) (them ^. position) (me ^. position)
                                              & each %~ signum
                                 in me & velocity %~ applyTriples (+) deltaV
    move = fmap $ \p -> p & position %~ applyTriples (+) (p ^. velocity)

energy :: [Planet P3] -> Int
energy = sum . fmap (liftA2 (*) kinetic potential)
  where
    kinetic = views position e
    potential = views velocity e
    e = sum . fmap abs . toListOf each

pt1 = Just . energy . unsafeHead . drop 1000 . iterate step

decomposePlanet :: Planet P3 -> (Planet Int, Planet Int, Planet Int)
decomposePlanet P{_position=(p1,p2,p3),_velocity=(v1,v2,v3)} =
  ( P p1 v1
  , P p2 v2
  , P p3 v3
  )

recomposePlanet :: (Planet Int, Planet Int, Planet Int) -> Planet P3
recomposePlanet ( P{_position=p1,_velocity=v1}
                , P{_position=p2,_velocity=v2}
                , P{_position=p3,_velocity=v3}
                ) = P (p1,p2,p3) (v1,v2,v3)

step' :: [Planet Int] -> [Planet Int]
step' planets = planets
               & applyAllGravity
               & move
  where
    applyAllGravity = fmap $ \p -> foldl' applyPlanetGravity p planets
    applyPlanetGravity me them = let deltaV = signum $ (them ^. position) - (me ^. position)
                                 in me & velocity +~ deltaV
    move = fmap $ \p -> p & position +~ (p ^. velocity)

pt2 :: Input -> Maybe Int
pt2 planets = Just
              $ lcm pz $ lcm px py
  where
    (px,py,pz) = over each findPeriod . unzip3 . fmap decomposePlanet $ planets
    findPeriod ps = fst
                    . unsafeHead
                    . dropWhile ((/= ps) . snd)
                    . drop 1
                    . zip [0..]
                    . iterate step'
                    $ ps



p12 :: Puzzle
p12 = Puzzle "12" inputParser pt1 pt2
