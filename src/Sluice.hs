{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Sluice where

import Sluice.Axis
import Sluice.Canvas
import Sluice.Scale

import Prelude hiding (mapM_)
import qualified Graphics.Blank as C
import Control.Applicative
import Control.Lens
import Data.Default.Class
import Data.Foldable
import Linear

class Plot p where
    plot :: p -> C.Canvas ()

data ScatterPlot = ScatterPlot {
    _xs :: [Double],
    _ys :: [Double],
    _xAxis :: RealAxis,
    _yAxis :: RealAxis,
    _marker :: Marker
    }

makeLenses ''ScatterPlot

scatter :: ScatterPlot
scatter = ScatterPlot [] [] def def def

instance Default ScatterPlot where
    def = scatter

instance Plot ScatterPlot where
    plot p = do
        me <- C.myCanvasContext
        let
            dataScale = V2 (_forward $ _scale (_xAxis p) xDomain) (_forward $ _scale (_yAxis p) yDomain)
            outputScale = (*) <$> V2 (C.width me - fst offset) (C.height me - snd offset)
            fullScale = liftA2 (.) outputScale dataScale
            pts = zipWith (\x y -> V2 x y) (_xs p) (_ys p)
            xDomain = _domain (_xAxis p) $ _xs p
            yDomain = _domain (_yAxis p) $ _ys p
            offset = (axisSize $ _yAxis p, axisSize $ _xAxis p)
        -- draw data markers
        C.saveRestore $ do
            C.translate offset
            withStyle (p ^. marker) $ mapM_ (draw $ p ^. marker) $ map (fullScale <*>) pts
        -- draw Axes
            -- drawAxis $ _xAxis p
            -- drawAxis $ _yAxis p
