{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Sluice.Axis where

import Graphics.Blank
import Sluice.Scale

import Data.Default.Class
import Data.Text
import Numeric.Interval
import Data.Foldable
import Data.Traversable
import Control.Lens

newtype Ticks a = Ticks [(a, Maybe Text)]
  deriving (Foldable, Functor, Traversable)

-- | TickFun is an alias for functions which determine the tick marks
-- on an Axis.  The parameters passed, and hence the definition, are
-- likely to change in future.
type TickFun a = Interval a -> Ticks a

withShow :: Show a => a -> (a, Maybe Text)
withShow a = (a, Just . pack . show $ a)

threeTicks :: (Show a, Fractional a) => TickFun a
threeTicks i = Ticks $ fmap withShow [inf i, midpoint i, sup i]

data RealAxis = RealAxis {
    _scale :: Scale Double,
    _ticks :: TickFun Double,
    _minorLength Double,
    _majorLength Double,
    _label :: Text,
    _textSize :: Double
    }

class Labeled a where
    label :: Lens' a Text
    textSize :: Lens' a Double

instance Labeled RealAxis where
    label = lens _label $ \s a -> s { _label = a }
    textSize = lens _textSize $ \s a -> s {_textSize = a }

makeLensesFor [("_scale", "scale"), ("_ticks", "ticks")] ''RealAxis

instance Default RealAxis where
    def = RealAxis def threeTicks "" 12

axisSize :: RealAxis -> Double
axisSize a = 2 * _textSize a + max (_majorLength p) (_minorLength p)

drawAxis :: RealAxis -> Canvas ()
drawAxis a = do
