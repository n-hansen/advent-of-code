module AoC2018.P10 (p10) where

import           AoC2018
import           Universum                  hiding (many)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

p10 :: Puzzle
p10 = Puzzle "10" inputParser (pure pt1) (pure pt2)

type Coord = (Int,Int)

data Point = P { position :: Coord
               , velocity :: Coord
               } deriving (Show,Eq)

type Input = [Point]

inputParser :: Parser Input
inputParser = point `endBy` newline
  where
    point = do
      string "position="
      p <- coordinate
      string "velocity="
      v <- coordinate
      pure $ P p v
    coordinate = do
      char '<'
      spaces
      x <- number
      char ','
      spaces
      y <- number
      char '>'
      spaces
      pure (x,y)
    number = L.signed space L.decimal
    spaces = many $ char ' '

tickPoint :: Point -> Point
tickPoint p@P{position=(x,y),velocity=(u,v)} = p {position=(x+u,y+v)}
tickAll :: Functor f => f Point -> f Point
tickAll = fmap tickPoint

boundingBox :: [Point] -> (Int,Int,Int,Int)
boundingBox (P{position=(x0,y0)}:ps) = go ps x0 x0 y0 y0
  where
    go [] xMin xMax yMin yMax = (xMin,xMax,yMin,yMax)
    go (P{position=(x,y)}:ps) xMin xMax yMin yMax =
      go ps
      (if x < xMin then x else xMin)
      (if x > xMax then x else xMax)
      (if y < yMin then y else yMin)
      (if y > yMax then y else yMax)

area :: (Int,Int,Int,Int) -> Int
area (xMin,xMax,yMin,yMax) = (xMax - xMin) * (yMax - yMin)

localMinimum :: (a -> a -> Ordering) -> [a] -> a
localMinimum _ [x] = x
localMinimum cmp (x1:x2:xs) | cmp x1 x2 == LT = x1
                            | otherwise = localMinimum cmp (x2:xs)

pprintPoints :: [Point] -> Text
pprintPoints pts = go locs (minX,minY,"")
  where
    go [] cursor@(x,_,out) | x == minX = out
                           | otherwise = go [] (advance blank cursor)
    go ps@((px,py):pRest) cursor@(cx,cy,_)
      | px == cx && py == cy = go pRest (advance filled cursor)
      | otherwise = go ps (advance blank cursor)
    blank = "."
    filled = "#"
    advance mark (x,y,out) =
      let
        newline = x == maxX
        out' = out <> mark <> if newline then "\n" else ""
        (x',y') = if newline
                  then (minX,y+1)
                  else (x+1,y)
      in
        (x',y',out')
    (minX,maxX,minY,_) = boundingBox pts
    locs = hashNub . sortOn (\(x,y)->(y,x)) . fmap position $ pts

pt1 :: Input -> Text
pt1 = pprintPoints . localMinimum (compare `on` area . boundingBox) . iterate tickAll

pt2 :: Input -> Text
pt2 = show . fst . localMinimum (compare `on` area . boundingBox . snd) . zip [0..] . iterate tickAll

exampleText = "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>\n"
exampleInput = fromMaybe [] . parseMaybe inputParser $ exampleText
exampleOutput1 = pt1 exampleInput
exampleOutput2 = pt2 exampleInput
