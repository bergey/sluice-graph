{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Sluice.Axis where

import Graphics.Blank
import Sluice.Scale

import Data.Default.Class
import Data.Text (Text, pack)
import Numeric.Interval.Kaucher
import Data.Foldable
import Data.List
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
    _scale :: Interval Double -> Scale Double,
    _ticks :: TickFun Double,
    _domain :: [Double] -> Interval Double,
    _minorLength :: Double,
    _majorLength :: Double,
    _label :: Text,
    _textSize :: Double
    }

class Labeled a where
    label :: Lens' a Text
    textSize :: Lens' a Double

instance Labeled RealAxis where
    label = lens _label $ \s a -> s { _label = a }
    textSize = lens _textSize $ \s a -> s {_textSize = a }

makeLensesFor [("_scale", "scale"), ("_ticks", "ticksFun")] ''RealAxis

ticks :: Setter' RealAxis (Ticks Double)
ticks = sets (\f ax -> ax { _ticks = \i -> f . _ticks ax $ i})

-- | The obvious domain function, @calcDomain xs == minimum xs .. maximum xs@.
calcDomain :: Ord a => [a] -> Interval a
calcDomain = foldl1' hull . map singleton

instance Default RealAxis where
    def = RealAxis linearScale threeTicks calcDomain 5 10 "" 12

axisSize :: RealAxis -> Double
axisSize a = 2 * _textSize a + max (_majorLength a) (_minorLength a)

drawAxis :: RealAxis -> Canvas ()
drawAxis _ = return ()
