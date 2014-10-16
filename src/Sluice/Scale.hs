{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Sluice.Scale where

import Control.Lens
import Data.Default.Class
import Numeric.Interval.Kaucher

data Scale a = Scale {
    _forward :: a -> Double,
    _backward :: Double -> a
    }

makeLenses ''Scale

instance Default (Scale Double) where
    def = Scale id id

-- TODO add some padding so no points are actually at 0 or 1
-- TODO (harder) make `Interval -> Scale` functions take params, like padding %
-- | The default linear scale maps the lower bound of the domain to 0,
-- and the high end to 1.
linearScale :: Interval Double -> Scale Double
linearScale d = Scale (\a -> (a - inf d) / width d) (\b -> width d * b + inf d)
