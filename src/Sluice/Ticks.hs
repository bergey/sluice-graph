{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Sluice.Ticks where

import Sluice.Text

import qualified Data.Text as T
import Data.Default.Class
import Data.Text (Text)
import Numeric.Interval.Kaucher
import Control.Lens

type TickMarks a = [(a, Maybe Text)]

-- | TickFun is an alias for functions which determine the tick marks
-- on an Axis.  The parameters passed, and hence the definition, are
-- likely to change in future.
data Ticks a = Ticks {
    _chooseTicks :: Interval a -> TickMarks a,
    _ticksLabel :: Label
    }

makeLenses ''Ticks

-- | Set (or modify) the tick positions without access to the domain Interval.
positions :: Setter' (Ticks a) (TickMarks a)
positions = sets $ \f -> chooseTicks %~ (f .)

instance HasLabel (Ticks a) where label = ticksLabel

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

threeTicks :: (Show a, RealFloat a) => Interval a -> TickMarks a
threeTicks i = fmap withShow [low 2 $ inf i, mid 2 $ midpoint i, high 2 $ sup i]

instance (Show a, RealFloat a) => Default (Ticks a) where
    def = Ticks threeTicks def

ticksOver :: Interval a -> Getter (Ticks a) (TickMarks a)
ticksOver i = chooseTicks . to ($ i)
