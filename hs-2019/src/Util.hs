module Util where

import Data.List

window :: Int -> [a] -> [[a]]
window n = filter ((n ==) . length) . fmap (take n) . tails

window2 :: [a] -> [(a,a)]
window2 = catMaybes
          . fmap (\case
                     [x,y] -> Just (x,y)
                     _     -> Nothing
                 )
          . window 2

window3 :: [a] -> [(a,a,a)]
window3 = catMaybes
          . fmap (\case
                     [x,y,z] -> Just (x,y,z)
                     _       -> Nothing
                 )
          . window 3
