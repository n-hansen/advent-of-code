module P16Spec where

import TestPrelude
import Parse

import Puzzles.P16 (Packet(..),PacketData(..))
import qualified Puzzles.P16 as P

pt1 = puzzleExample P.inputParser P.pt1 "pt 1"
pt2 = puzzleExample P.inputParser P.pt2 "pt 2"

shouldParseAsPacket input expect =
  (parseEither P.inputParser input >>= parseEither P.topLevelPacket)
  `shouldBe`
  Right expect

spec_p16 :: Spec
spec_p16 = do
  describe "parser" $ do
    specify "literal" $
      "D2FE28"
      `shouldParseAsPacket`
      Packet 6 (Literal 2021)
    specify "type 0 operator" $
      "38006F45291200"
      `shouldParseAsPacket`
      Packet 1 (Operator 6 [Packet 6 $ Literal 10, Packet 2 $ Literal 20])
    specify "type 1 operator" $
      "EE00D40C823060"
      `shouldParseAsPacket`
      Packet 7 (Operator 3 [ Packet 2 $ Literal 1
                           , Packet 4 $ Literal 2
                           , Packet 1 $ Literal 3
                           ])

  describe "example 1" $ do
    let input = [r|8A004A801A8002F478|]

    pt1 input 16
  describe "example 2" $ do
    let input = "9C0141080250320F1802104A08"
    pt2 input 1
