module Parse
  ( module X
  , unsignedInteger
  , signedInteger
  , actualSpaces
  , actualSpaces1
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
