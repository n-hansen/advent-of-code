module Puzzles.P18 where

import AocPrelude hiding (left,right,r,reduce,magnitude)
import Parse
import Puzzle

data SNumber = Pair SNumber SNumber
             | RegularNumber Int
             deriving (Eq,Show)

data SZipper = SZ SNumber [SContext]
             deriving (Show)

data SContext = Top | PairL SNumber | PairR SNumber
              deriving (Show)

zipper :: SNumber -> SZipper
zipper sn = SZ sn [Top]

left,right,up,down,next :: SZipper -> Maybe SZipper
left (SZ _ (Top:_)) = Nothing
left (SZ r (PairR l:ctx)) = Just $ SZ l (PairL r:ctx)
left z = up z >>= left
right (SZ _ (Top:_)) = Nothing
right (SZ l (PairL r:ctx)) = Just $ SZ r (PairR l:ctx)
right z = up z >>= right
up (SZ l (PairL r:ctx)) = Just $ SZ (Pair l r) ctx
up (SZ r (PairR l:ctx)) = Just $ SZ (Pair l r) ctx
up _ = Nothing
down (SZ (Pair l r) ctx) = Just $ SZ l (PairL r:ctx)
down _ = Nothing
next z = down z <|> right z

edit :: (SNumber -> SNumber) -> SZipper -> SZipper
edit f (SZ loc ctx) = SZ (f loc) ctx

editNum :: (Int -> Int) -> SZipper -> Maybe SZipper
editNum f (SZ (RegularNumber x) ctx) = Just $ SZ (RegularNumber $ f x) ctx
editNum _ _ = Nothing

editLNum, editRNum :: (Int -> Int) -> SZipper -> SZipper
editLNum f (SZ loc ctx) = SZ loc (editContext ctx)
  where
    applyToRightmost (RegularNumber x) = RegularNumber $ f x
    applyToRightmost (Pair l r) = Pair l (applyToRightmost r)

    editContext (PairL r:ctx) = PairL r:editContext ctx
    editContext (PairR l:ctx) = PairR (applyToRightmost l):ctx
    editContext top = top

editRNum f (SZ loc ctx) = SZ loc (editContext ctx)
  where
    applyToLeftmost (RegularNumber x) = RegularNumber $ f x
    applyToLeftmost (Pair l r) = Pair (applyToLeftmost l) r

    editContext (PairR l:ctx) = PairR l:editContext ctx
    editContext (PairL r:ctx) = PairL (applyToLeftmost r):ctx
    editContext top = top

node,root :: SZipper -> SNumber
node (SZ n _) = n
root loc = case up loc of
             Just parent -> root parent
             Nothing -> node loc

p18 :: Puzzle
p18 = Puzzle "18" inputParser pt1 pt2

type Input = [SNumber]

inputParser :: Parser Input
inputParser = snParser `endBy` newline

snParser :: Parser SNumber
snParser = pair
  where
    child = pair <|> number
    number = RegularNumber <$> signedInteger
    pair = do
      string "["
      l <- child
      string ","
      r <- child
      string "]"
      pure $ Pair l r


reduce :: SNumber -> Maybe SNumber
reduce sn = explode sn <|> split sn

explode :: SNumber -> Maybe SNumber
explode = head
          . catMaybes
          . unfoldr (fmap (tryExplode &&& identity) . next)
          . zipper
  where
    tryExplode (SZ (Pair (RegularNumber l) (RegularNumber r)) ctx)
      | length ctx >= 5 = Just
                          $ SZ (RegularNumber 0) ctx
                          & editLNum (+ l)
                          & editRNum (+ r)
                          & root
    tryExplode _ = Nothing


split = head
        . catMaybes
        . unfoldr (fmap (trySplit &&& identity) . next)
        . zipper
  where
    trySplit (SZ (RegularNumber x) ctx)
      | x >= 10 = Just . root $ SZ (splitPair x) ctx
    trySplit _ = Nothing
    splitPair x = let (d,m) = x `divMod` 2
                  in Pair (RegularNumber d) (RegularNumber $ d + m)

fullyReduce sn = case reduce sn of
                   Just sn' -> fullyReduce sn'
                   Nothing -> sn

add x y = fullyReduce $ Pair x y

magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r
magnitude (RegularNumber x) = x

pt1 = fmap magnitude . foldl1May' add

pt2 numbers = Just . maximum $ do
  (i,x) <- zip [1..] numbers
  (j,y) <- zip [1..] numbers
  guard $ i /= j
  pure $ magnitude $ add x y
