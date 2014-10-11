{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Sluice.Scale where

import Control.Lens
import Data.Default.Class

data Scale a = Scale {
    _forward :: a -> Double,
    _backward :: Double -> a
    }

makeLenses ''Scale

instance Default (Scale Double) where
    def = Scale id id
