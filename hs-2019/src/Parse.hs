module Parse
  ( module X
  , unsignedInteger
  , signedInteger
  , Parser
  ) where

import           Text.Megaparsec as X
import           Text.Megaparsec.Char as X
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

unsignedInteger :: Parser Int
unsignedInteger = L.decimal

signedInteger :: Parser Int
signedInteger = L.signed (pure ()) L.decimal
