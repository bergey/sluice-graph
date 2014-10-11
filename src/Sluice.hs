{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Sluice where

import Sluice.Axis
import Sluice.Canvas

import Prelude hiding (mapM_)
import Graphics.Blank (Canvas)
import Control.Lens
import Data.Default.Class
import Data.Foldable
import Linear
import Linear.Affine

class Plot p where
    plot :: p a -> a -> Canvas ()

data ScatterPlot a = ScatterPlot {
    _xs :: a -> [Double],
    _ys :: a -> [Double],
    _xAxis :: RealAxis,
    _yAxis :: RealAxis,
    _marker :: Marker
    }

makeLenses ''ScatterPlot

scatter :: Foldable f => ScatterPlot (f Double, f Double)
scatter = ScatterPlot (toList . fst) (toList . snd) def def def

instance Foldable f => Default (ScatterPlot (f Double, f Double)) where
    def = scatter

instance Plot ScatterPlot where
    plot p d = do
        let
            pts = zipWith (\x y -> P $ V2 x y) (_xs p d) (_ys p d)
        saveRestore $ do
            translate(axisSize $ _yAxis p, axisSize $ _xAxis p)
            withStyle (p ^. marker) $ mapM_ (draw $ p ^. marker) pts
        drawAxis
