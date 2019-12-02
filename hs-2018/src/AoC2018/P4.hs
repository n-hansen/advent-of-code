{-# LANGUAGE NamedFieldPuns #-}
module AoC2018.P4 (p4) where

import           AoC2018
import           Universum

import           Data.IntMap.Strict         (IntMap)
import qualified Data.IntMap.Strict         as Map
import           Data.IntMultiSet           (IntMultiSet)
import qualified Data.IntMultiSet           as MSet
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

p4 :: Puzzle
p4 = Puzzle "4" inputParser (pure pt1) (pure pt2)

data Timestamp = TS { tsYear   :: Int
                    , tsMonth  :: Int
                    , tsDay    :: Int
                    , tsHour   :: Int
                    , tsMinute :: Int
                    } deriving (Show,Eq,Ord)

data LogEntry = ShiftBegins Timestamp Int
              | FallsAsleep Timestamp
              | WakesUp Timestamp
              deriving (Show,Eq)

getTimestamp :: LogEntry -> Timestamp
getTimestamp (ShiftBegins ts _) = ts
getTimestamp (FallsAsleep ts)   = ts
getTimestamp (WakesUp ts)       = ts

instance Ord LogEntry where
  compare = compare `on` getTimestamp

type Input = [LogEntry]

inputParser :: Parser Input
inputParser = parseLogEntry `endBy` newline
  where
    parseLogEntry :: Parser LogEntry
    parseLogEntry = do
      ts <- parseTimestamp
      parseShiftBegins ts
        <|> parseFallsAsleep ts
        <|> parseWakesUp ts

    parseTimestamp :: Parser Timestamp
    parseTimestamp = do
      char '['
      year <- L.decimal
      char '-'
      month <- L.decimal
      char '-'
      day <- L.decimal
      space
      hour <- L.decimal
      char ':'
      minute <- L.decimal
      char ']'
      space
      pure $ TS year month day hour minute

    parseShiftBegins :: Timestamp -> Parser LogEntry
    parseShiftBegins ts = ShiftBegins ts <$>
                          between
                          (string "Guard #")
                          (string " begins shift")
                          L.decimal

    parseFallsAsleep :: Timestamp -> Parser LogEntry
    parseFallsAsleep ts = string "falls asleep" >> pure (FallsAsleep ts)

    parseWakesUp :: Timestamp -> Parser LogEntry
    parseWakesUp ts = string "wakes up" >> pure (WakesUp ts)

testParse = parseTest inputParser input
  where
    input = unlines [ "[1518-11-01 00:00] Guard #10 begins shift"
                    , "[1518-11-01 00:05] falls asleep"
                    , "[1518-11-01 00:25] wakes up"
                    ]

type GuardHistories = IntMap IntMultiSet

pt1 :: Input -> Text
pt1 logs = show $ sleepiestGuard * sleepiestMinute
  where
    hist = analyzeLogs logs
    sleepiestGuard = snd $
                     Map.foldlWithKey' (\old@(max,_) g naps ->
                                           let
                                             napLength = MSet.size naps
                                           in
                                             if napLength > max
                                             then (napLength,g)
                                             else old
                                       )
                     (0, error "never found a sleepy guard")
                     hist
    sleepiestMinute = fst . findMostFrequent $ hist Map.! sleepiestGuard

pt2 :: Input -> Text
pt2 logs = show $ guard * minute
  where
    (guard,minute,_) = Map.foldlWithKey' (\old@(_,_,max) g (m,cnt) ->
                                            if cnt > max
                                            then (g,m,cnt)
                                            else old
                                         ) (error "guard error",error "minute error",0)
                       . fmap findMostFrequent
                       . analyzeLogs
                       $ logs

analyzeLogs :: [LogEntry] -> GuardHistories
analyzeLogs = go Map.empty (error "no one ever started a shift") . sort
  where
    go :: GuardHistories -> Int -> [LogEntry] -> GuardHistories
    go hist _ [] = hist
    go hist _ (ShiftBegins _ g:rest) = go hist g rest
    go hist g (FallsAsleep s:WakesUp w:rest) = go (recordNap g (tsMinute s) (tsMinute w) hist) g rest
    go hist g (FallsAsleep s:ShiftBegins _ g2:rest) = go (recordNap g (tsMinute s) 60 hist) g2 rest
    go _ _ logs = error $ "missing state transition" <> show (take 5 logs)
    recordNap g s w =
      Map.alter (\naps -> Just $ MSet.fromList [s..w-1] <> fromMaybe mempty naps) g

findMostFrequent :: IntMultiSet -> (Int,Int)
findMostFrequent = MSet.foldOccur
                   (\val occ old@(_,max) ->
                       if occ > max
                       then (val,occ)
                       else old
                   )
                   (error "multiset frequency error",0)
