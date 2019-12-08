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
  , initProgram
  , provideInput
  , tapeParser
  , tape
  , position
  , inputStream
  , outputStream
  ) where

import Parse
import Util

import           Control.Monad.Trans.Maybe
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vec


----- TYPES -----

newtype Addr = Addr { unAddr :: Int } deriving (Eq,Show,Ord,Enum,Generic)
newtype Val = Val { unVal :: Int } deriving (Eq,Show,Ord,Enum,Generic)
instance Wrapped Addr where
instance Wrapped Val where

type Tape = Vector Int

data ComputerState = CS { _position :: !Addr
                        , _inputStream :: [Int]
                        , _outputStream :: [Int]
                        , _tape :: !Tape
                        } deriving (Eq, Show)
makeLenses ''ComputerState

type Compute = State ComputerState

data Status = Continue | BlockedOnInput | Done
            deriving (Eq,Show)

type OpView = Maybe (Val, [OpArg])
data OpArg = PositionMode Addr Val
           | ImmediateMode Val
           deriving (Eq,Show)

newtype RunResult = RunResult (Status, ComputerState) deriving Show

----- TOP LEVEL API -----

-- ComputerState views
pattern Tape t <- (view tape -> t)
pattern Output o <- (view outputStream -> o)
pattern DrainedOutput o st <- (view outputStream &&& outputStream %~ mempty -> (o, st))

-- ProgramResult views
pattern Halted s <- RunResult (Done, s)
pattern WaitingForInput s <- RunResult (BlockedOnInput, s)


runProgram :: ComputerState -> RunResult
runProgram = RunResult . runState mainLoop

initProgram listing = CS (Addr 0) [] [] listing

provideInput :: [Int] -> ComputerState -> ComputerState
provideInput input = inputStream %~ (<> input)

tapeParser :: Parser Tape
tapeParser = fmap Vec.fromList $ signedInteger `sepBy` "," <* optional newline

----- INTERNAL IMPL -----

mainLoop :: Compute Status
mainLoop = do
  status <- eval =<< get
  case status of
    Continue -> mainLoop
    _        -> pure status

argVal (PositionMode _ (Val v)) = Just v
argVal (ImmediateMode (Val v)) = Just v

pattern ArgVal v <- (argVal -> Just v)
pattern ArgAddr a <- PositionMode a _

atAddr (Addr addr) = tape . ix addr

deref :: ComputerState -> Addr -> Maybe Int
deref st addr = st ^? atAddr addr

readArg :: ComputerState -> Int -> Addr -> Maybe OpArg
readArg st mode addr = do
  atPos <- st `deref` addr
  case mode of
    0 -> PositionMode (Addr atPos) . Val <$> st `deref` Addr atPos
    1 -> pure . ImmediateMode . Val $ atPos
    _ -> Nothing

viewOp :: ComputerState -> OpView
viewOp st = do
  let pos = st ^. position
  (argModes, op) <- first (unfoldr $ Just . swap . (`divMod` 10))
                    . (`divMod` 100)
                    <$> st `deref` pos
  pure ( Val op
       , catMaybes
         . takeWhile isJust
         . zipWith (readArg st) argModes
         $ [succ pos..]
       )

pattern Op0 op <- Just (Val op, _)
pattern Op1 op arg1 <- Just (Val op, arg1:_)
pattern Op2 op arg1 arg2 <- Just (Val op, arg1:arg2:_)
pattern Op3 op arg1 arg2 arg3 <- Just (Val op, arg1:arg2:arg3:_)


advance :: Int -> Compute ()
advance n = position . _Wrapped' += n

eval :: ComputerState -> Compute Status
eval st
  -- 1: add
  | Op3 1 (ArgVal v1) (ArgVal v2) (ArgAddr out) <- op
  = do
      atAddr out .= v1 + v2
      advance 4
      pure Continue

  -- 2: mul
  | Op3 2 (ArgVal v1) (ArgVal v2) (ArgAddr out) <- op
  = do
      atAddr out .= v1 * v2
      advance 4
      pure Continue

  -- 3: read input
  | Op1 3 (ArgAddr out) <- op
  , Just input <- st ^? inputStream . _head
  = do
      atAddr out .= input
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
      atAddr out .= (if v1 < v2 then 1 else 0)
      advance 4
      pure Continue

  -- 8: eq
  | Op3 8 (ArgVal v1) (ArgVal v2) (ArgAddr out) <- op
  = do
      atAddr out .= (if v1 == v2 then 1 else 0)
      advance 4
      pure Continue

  -- 99: halt
  | Op0 99 <- op
  = pure Done

  where
    op = viewOp st
