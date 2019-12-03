module Parse
  ( module X
  , signedInteger
  , Parser
  ) where

import           Text.Megaparsec as X
import           Text.Megaparsec.Char as X
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

signedInteger :: Parser Int
signedInteger = L.signed (pure ()) L.decimal
