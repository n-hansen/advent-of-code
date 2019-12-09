{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module IntcodeComputer
  ( ComputerState
  , Tape
  , RunResult
  , pattern Tape
  , pattern Output
  , pattern DrainedOutput
  , pattern Halted
  , pattern WaitingForInput
  , runProgram
  , runUntilHalt
  , initProgram
  , provideInput
  , tapeParser
  , tape
  , position
  , inputStream
  , outputStream
  ) where

import Parse

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM


----- TYPES -----

newtype Addr = Addr { unAddr :: Int } deriving (Eq,Show,Ord,Enum,Generic)
newtype Val = Val { unVal :: Int } deriving (Eq,Show,Ord,Enum,Generic)
instance Wrapped Addr where
instance Wrapped Val where

type Tape = [Int]
type InternalTape =  IntMap Int

data ComputerState = CS { _position :: !Addr
                        , _relativeBase :: !Addr
                        , _inputStream :: [Int]
                        , _outputStream :: [Int]
                        , _tape :: !InternalTape
                        } deriving (Eq, Show)
makeLenses ''ComputerState

type Compute = State ComputerState

data Status = Continue | BlockedOnInput | Done
            deriving (Eq,Show)

type OpView = (Val, [OpArg])
data OpArg = PositionMode Addr Val
           | RelativeMode Addr Val
           | ImmediateMode Val
           deriving (Eq,Show)

newtype RunResult = RunResult (Status, ComputerState) deriving Show

----- TOP LEVEL API -----

-- ComputerState views
pattern Tape t <- (views tape unrollTape -> t)
pattern Output o <- (view outputStream -> o)
pattern DrainedOutput o st <- (view outputStream &&& outputStream %~ mempty -> (o, st))

-- ProgramResult views
pattern Halted s <- RunResult (Done, s)
pattern WaitingForInput s <- RunResult (BlockedOnInput, s)


runProgram :: ComputerState -> RunResult
runProgram = RunResult . runState mainLoop

initProgram :: Tape -> ComputerState
initProgram = CS (Addr 0) (Addr 0) [] [] . IM.fromAscList . zip [0..]

runUntilHalt :: Tape -> [Int] -> Maybe [Int]
runUntilHalt init input =
  case initProgram init
       & provideInput input
       & runProgram
  of Halted (Output o) -> Just o
     _ -> Nothing

provideInput :: [Int] -> ComputerState -> ComputerState
provideInput input = inputStream %~ (<> input)

tapeParser :: Parser Tape
tapeParser = signedInteger `sepBy` "," <* optional newline

----- INTERNAL IMPL -----

mainLoop :: Compute Status
mainLoop = do
  status <- eval =<< get
  case status of
    Continue -> mainLoop
    _        -> pure status

unrollTape :: IntMap Int -> [Int]
unrollTape = take 1000 . fillBlanks 0 . IM.toAscList
  where
    fillBlanks _ [] = repeat 0
    fillBlanks i xs@((j,x):ys) =
      if i == j
      then x : fillBlanks (i+1) ys
      else 0 : fillBlanks (i+1) xs

argVal (PositionMode _ (Val v)) = v
argVal (ImmediateMode (Val v)) = v
argVal (RelativeMode _ (Val v)) = v

argAddr (PositionMode a _) = Just a
argAddr (RelativeMode a _) = Just a
argAddr _ = Nothing

pattern ArgVal v <- (argVal -> v)
pattern ArgAddr a <- (argAddr -> Just a)

atAddr (Addr addr) = tape . at addr

deref :: ComputerState -> Addr -> Int
deref st addr = st ^. atAddr addr . non 0

readArg :: ComputerState -> Int -> Addr -> Maybe OpArg
readArg st mode addr = do
  let atPos = st `deref` addr
  case mode of
    0 -> pure . PositionMode (Addr atPos) . Val $ st `deref` Addr atPos
    1 -> pure . ImmediateMode . Val $ atPos
    2 -> let absAddr = st ^. relativeBase . to (_Wrapped' +~ atPos)
         in pure . RelativeMode absAddr . Val $ st `deref` absAddr
    _ -> Nothing

viewOp :: ComputerState -> OpView
viewOp st = let pos = st ^. position
                (argModes, op) = first (unfoldr $ Just . swap . (`divMod` 10))
                                 . (`divMod` 100)
                                 $ st `deref` pos
            in ( Val op
               , catMaybes
                 . takeWhile isJust
                 . zipWith (readArg st) argModes
                 $ [succ pos..]
               )

pattern Op0 op <- (Val op, _)
pattern Op1 op arg1 <- (Val op, arg1:_)
pattern Op2 op arg1 arg2 <- (Val op, arg1:arg2:_)
pattern Op3 op arg1 arg2 arg3 <- (Val op, arg1:arg2:arg3:_)

writeTo :: Addr -> Int -> Compute ()
writeTo addr val = atAddr addr .= Just val

advance :: Int -> Compute ()
advance n = position . _Wrapped' += n

eval :: ComputerState -> Compute Status
eval st
  -- 1: add
  | Op3 1 (ArgVal v1) (ArgVal v2) (ArgAddr out) <- op
  = do
      writeTo out $ v1 + v2
      advance 4
      pure Continue

  -- 2: mul
  | Op3 2 (ArgVal v1) (ArgVal v2) (ArgAddr out) <- op
  = do
      writeTo out $ v1 * v2
      advance 4
      pure Continue

  -- 3: read input
  | Op1 3 (ArgAddr out) <- op
  , Just input <- st ^? inputStream . _head
  = do
      writeTo out input
      inputStream %= drop 1
      advance 2
      pure Continue

  | Op1 3 _ <- op
  , Nothing <- st ^? inputStream . _head
  = pure BlockedOnInput

  -- 4: write output
  | Op1 4 (ArgVal v) <- op
  = do
      outputStream %= (<> pure v)
      advance 2
      pure Continue

  -- 5: jnz
  | Op2 5 (ArgVal v1) (ArgVal v2) <- op
  = do
      if v1 /= 0
        then position .= Addr v2
        else advance 3
      pure Continue

  -- 6: jez
  | Op2 6 (ArgVal v1) (ArgVal v2) <- op
  = do
      if v1 == 0
        then position .= Addr v2
        else advance 3
      pure Continue

  -- 7: lt
  | Op3 7 (ArgVal v1) (ArgVal v2) (ArgAddr out) <- op
  = do
      writeTo out $ if v1 < v2 then 1 else 0
      advance 4
      pure Continue

  -- 8: eq
  | Op3 8 (ArgVal v1) (ArgVal v2) (ArgAddr out) <- op
  = do
      writeTo out $ if v1 == v2 then 1 else 0
      advance 4
      pure Continue

  -- 9: set relative base
  | Op1 9 (ArgVal v) <- op
  = do
      relativeBase . _Wrapped' += v
      advance 2
      pure Continue

  -- 99: halt
  | Op0 99 <- op
  = pure Done

  where
    op = viewOp st
