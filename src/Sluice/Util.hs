module Sluice.Util where

import qualified Data.Text as T

text :: Show a => a -> T.Text
text = T.pack . show
