module Parse
  ( module X
  , unsignedInteger
  , signedInteger
  , actualSpaces
  , actualSpaces1
  , singleDigitInt
  , Parser
  ) where

import           AocPrelude hiding (some)

import           Text.Megaparsec as X hiding (State)
import           Text.Megaparsec.Char as X
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

unsignedInteger :: Parser Int
unsignedInteger = L.decimal

signedInteger :: Parser Int
signedInteger = L.signed (pure ()) L.decimal

actualSpaces :: Parser ()
actualSpaces = many (string " ") >> pure ()

actualSpaces1 :: Parser ()
actualSpaces1 = some (string " ") >> pure ()

singleDigitInt :: Parser Int
singleDigitInt = parseDigit <$> digitChar
  where
    parseDigit '0' = 0
    parseDigit '1' = 1
    parseDigit '2' = 2
    parseDigit '3' = 3
    parseDigit '4' = 4
    parseDigit '5' = 5
    parseDigit '6' = 6
    parseDigit '7' = 7
    parseDigit '8' = 8
    parseDigit '9' = 9
    _ = panic "unreachable"
