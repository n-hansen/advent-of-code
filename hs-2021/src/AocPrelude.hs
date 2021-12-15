module AocPrelude
  ( module X
  , showTxt
  ) where

import           Protolude                 as X hiding (to,from,uncons,unsnoc,(<.>),many,(%))
import           Protolude.Unsafe          as X
import           Protolude.Error           as X
import           Control.Arrow             as X hiding (first,second)
import           Prettyprinter             as X (Pretty(..),viaShow)
import           Optics                    as X
import           Text.RawString.QQ         as X

showTxt :: Show a => a -> Text
showTxt = show
