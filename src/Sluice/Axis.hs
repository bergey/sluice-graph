{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Sluice.Axis where

import Graphics.Blank
import Sluice.Scale
import Sluice.Canvas

import qualified Graphics.Blank as C

import Prelude hiding (mapM_)
import Data.Default.Class
import qualified Data.Text as T
import Data.Text (Text)
import Numeric.Interval.Kaucher
import Data.Foldable
import Data.List
import Data.Traversable
import Control.Lens

type Ticks a = [(a, Maybe Text)]

-- | TickFun is an alias for functions which determine the tick marks
-- on an Axis.  The parameters passed, and hence the definition, are
-- likely to change in future.
type TickFun a = Interval a -> Ticks a

withShow :: Show a => a -> (a, Maybe Text)
withShow a = (a, Just . T.pack . show $ a)

-- helper for low, mid, high
rounder :: RealFloat a => (a -> Int) -> Int -> a -> a
rounder f n x = fromIntegral (f $ x / 10 ** sigFigs) * 10 ** sigFigs where
  sigFigs = fromIntegral $ l - n +1
  l = floor $ logBase 10 x

-- | @low n x@ rounds x down to the next number with n significant digits
low :: RealFloat a => Int -> a -> a
low = rounder floor

-- | @mid n x@ rounds x to the nearest number with n significant digits
mid :: RealFloat a => Int -> a -> a
mid = rounder round

-- | @high n x@ rounds x up to the next number with n significant digits
high :: RealFloat a => Int -> a -> a
high = rounder ceiling

threeTicks :: (Show a, RealFloat a) => TickFun a
threeTicks i = fmap withShow [low 2 $ inf i, mid 2 $ midpoint i, high 2 $ sup i]

data RealAxis = RealAxis {
    _scale :: Interval Double -> Scale Double,
    _ticks :: TickFun Double,
    _domain :: [Double] -> Interval Double,
    _minorLength :: Double,
    _majorLength :: Double,
    _label :: Text,
    _textSize :: Double,
    _orientation :: Position,
    _textAngle :: Double
    }

data Position = Bottom | Top | Left | Right

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
    def = RealAxis linearScale threeTicks calcDomain 5 10 "" 12 Bottom 0

axisSize :: RealAxis -> Double
axisSize a = 2 * _textSize a + max (_majorLength a) (_minorLength a)

data AxisParams = AxisParams {
    _axDomain :: Interval Double,
    _axScale :: Double -> Double
    -- _outputScale :: Double -> Double
    }

instance Default AxisParams where
    def = AxisParams empty id

drawAxis :: AxisParams -> RealAxis -> Canvas ()
drawAxis p ax = do  -- only Location == Bottom case
    -- draw line length of plot
    C.beginPath()
    C.moveTo(0, axisSize ax)
    C.lineTo(_axScale p . sup $ _axDomain p, axisSize ax)
    -- draw ticks
    mapM_ (drawTick ax) ((_ticks ax (_axDomain p)) & mapped . _1 %~ _axScale p)
    C.stroke()

-- | drawTick expects pre-scaled coordinates in its second argument
drawTick :: RealAxis -> (Double, Maybe Text) -> Canvas ()
drawTick ax (x, label) = let
    baseline = axisSize ax
    tickLength = case label of
        Nothing -> _minorLength ax
        Just _ -> _majorLength ax
    in do
    -- tick mark
    C.moveTo (x, baseline)
    C.lineTo (x, baseline - tickLength)
    -- tick label
    case label of
     Just txt -> do
         (TextMetrics w) <- measureText txt
         -- invertY $ fillText (txt, x - w / 2, baseline - tickLength - _textSize ax)
         drawText txt (x-w/2) (baseline - tickLength - _textSize ax)
     _-> return ()
