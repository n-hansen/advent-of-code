{-# LANGUAGE TypeFamilies #-}
module Puzzles.P16 where

import AocPrelude hiding (sourceColumn)
import Parse
import Puzzle

import qualified GHC.Show
import qualified Data.Sequence as Seq
import qualified Data.String

newtype BitString = BitString { unwrapBitstring :: Seq Bool }
                  deriving (Eq,Ord,Semigroup,Monoid)

instance Show BitString where
  show = fmap (\case
                  True -> '1'
                  False -> '0'
              )
         . toList
         . unwrapBitstring

instance IsString BitString where
  fromString = BitString
               . Seq.fromList
               . fmap (\case
                          '1' -> True
                          '0' -> False
                          c -> panic $ "Unexpected character in bitstring: " <> show c
                      )

instance Stream BitString where
  type Token BitString = Bool
  type Tokens BitString = BitString
  tokenToChunk _ = BitString . pure
  tokensToChunk _ = BitString . Seq.fromList
  chunkToTokens _ = toList . unwrapBitstring
  chunkLength _ = length . unwrapBitstring
  take1_ (BitString Empty) = Nothing
  take1_ (BitString (t :< ts)) = Just (t, BitString ts)
  takeN_ n (BitString s)
    | n <= 0 = Just (BitString mempty, BitString s)
    | null s = Nothing
    | otherwise = Just . first BitString . second BitString $ (Seq.splitAt n s)
  takeWhile_ f = first BitString . second BitString . Seq.spanl f . unwrapBitstring
instance VisualStream BitString where
  showTokens _ (t :| ts) = show . BitString $ t <| Seq.fromList ts
instance TraversableStream BitString where
  reachOffset o st@PosState{pstateInput=(BitString s),pstateOffset,pstateSourcePos} =
    ( Nothing
    , st { pstateInput = BitString $ Seq.drop (o - pstateOffset) s
         , pstateOffset = o
         , pstateSourcePos = pstateSourcePos { sourceColumn = mkPos o }
         }
    )

p16 :: Puzzle
p16 = Puzzle "16" inputParser pt1 pt2

type Input = BitString

inputParser :: Parser Input
inputParser = mconcat <$> many hex <* optional newline
  where
    hex = choice [ "0000" <$ char '0'
                 , "0001" <$ char '1'
                 , "0010" <$ char '2'
                 , "0011" <$ char '3'
                 , "0100" <$ char '4'
                 , "0101" <$ char '5'
                 , "0110" <$ char '6'
                 , "0111" <$ char '7'
                 , "1000" <$ char '8'
                 , "1001" <$ char '9'
                 , "1010" <$ (char 'a' <|> char 'A')
                 , "1011" <$ (char 'b' <|> char 'B')
                 , "1100" <$ (char 'c' <|> char 'C')
                 , "1101" <$ (char 'd' <|> char 'D')
                 , "1110" <$ (char 'e' <|> char 'E')
                 , "1111" <$ (char 'f' <|> char 'F')
                 ]

data Packet = Packet { _version :: Int
                     , _pdata :: PacketData
                     } deriving (Eq,Show)

data PacketData = Literal Int
                | Operator Int (Seq Packet)
                deriving (Eq,Show)

type BitParser = Parsec Void BitString

bits2Int :: BitString -> Int
bits2Int = foldl' (\acc b ->
                    (if b then 1 else 0) + 2 * acc
                 ) 0
           . unwrapBitstring

packetParser :: BitParser Packet
packetParser = do
  v <- intValue 3
  t <- intValue 3
  Packet v <$> case t of
                 4 -> literalValue
                 t -> operator t

  where
    intValue n = bits2Int <$> takeP Nothing n

    literalValue :: BitParser PacketData
    literalValue = literalValue' 0
    literalValue' :: Int -> BitParser PacketData
    literalValue' acc = do
      continue <- anySingle
      acc' <- (16 * acc +) <$> intValue 4
      if continue then literalValue' acc' else pure $ Literal acc'

    operator :: Int -> BitParser PacketData
    operator t = do
      typeId <- anySingle
      Operator t <$> case typeId of
                       False -> operator0
                       True -> operator1

    operator0 = do
      subpacketLength <- intValue 15
      startOffset <- getOffset
      operator0' subpacketLength startOffset Empty

    operator0' bitsLeft startOffset subpackets = do
      subpacket <- packetParser
      newOffset <- getOffset
      let bitsConsumed = newOffset - startOffset
      if bitsConsumed >= bitsLeft
        then pure $ subpackets |> subpacket
        else operator0' (bitsLeft - bitsConsumed) newOffset (subpackets |> subpacket)

    operator1 = do
      subpacketCount <- intValue 11
      Seq.fromList <$> count subpacketCount packetParser

topLevelPacket = packetParser <* many (single False) <* eof

versionSum (Packet v (Literal _)) = v
versionSum (Packet v (Operator _ ps)) = v + (sum . fmap versionSum $ ps)

pt1 i = versionSum <$> parseMaybe topLevelPacket i

evalPacket :: Packet -> Int
evalPacket (Packet _ d) = eval d
  where
    eval (Literal x) = x
    eval (Operator op ps) = fmap evalPacket ps &
                            case op of
                              0 -> sum
                              1 -> product
                              2 -> minimum
                              3 -> maximum
                              5 -> greaterThan
                              6 -> lessThan
                              7 -> equalTo
    greaterThan (x :< y :< Empty) = if x > y then 1 else 0
    lessThan (x :< y :< Empty) = if x < y then 1 else 0
    equalTo (x :< y :< Empty) = if x == y then 1 else 0

pt2 i = evalPacket <$> parseMaybe topLevelPacket i
