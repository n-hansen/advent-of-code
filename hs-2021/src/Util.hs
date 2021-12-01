module Util where

import AocPrelude

import Control.Monad.Trans.Maybe
import Data.List

window :: Int -> [a] -> [[a]]
window n = filter ((n ==) . length) . fmap (take n) . tails

window2 :: [a] -> [(a,a)]
window2 = mapMaybe (\case
                       [x,y] -> Just (x,y)
                       _     -> Nothing
                   )
          . window 2

window3 :: [a] -> [(a,a,a)]
window3 = mapMaybe (\case
                       [x,y,z] -> Just (x,y,z)
                       _       -> Nothing
                   )
          . window 3

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = maybe (Left l) Right

liftedNothing :: Monad m => MaybeT m a
liftedNothing = MaybeT (pure Nothing)
