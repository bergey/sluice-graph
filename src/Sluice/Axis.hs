{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sluice.Axis where

import Sluice.Scale
import Sluice.Ticks
import Sluice.Text
import Sluice.Canvas as D

import qualified Graphics.Blank as C

import Prelude hiding (mapM_)
import Data.Default.Class
import qualified Data.Text as T
import Data.Text (Text)
import Numeric.Interval.Kaucher
import Data.Foldable
import Data.List
-- import Data.Traversable
import Control.Lens
import Linear

data RealAxis = RealAxis {
    _scale :: Interval Double -> Scale Double,
    _ticks :: Ticks Double,
    _domain :: [Double] -> Interval Double,
    _minorLength :: Double,
    _majorLength :: Double,
    _ticksTransform :: M22 Double, -- a transform from tick space to global coördinates
    _axisLabel :: Label
    }

makeLenses ''RealAxis

instance HasLabel RealAxis where label = axisLabel

-- | The obvious domain function, @calcDomain xs == minimum xs .. maximum xs@.
calcDomain :: Ord a => [a] -> Interval a
calcDomain = foldl1' hull . map singleton

instance Default RealAxis where
    def = RealAxis linearScale def calcDomain 5 10 eye2 def
          & labelBaseline .~ C.HangingBaseline
          & labelAnchor .~ C.CenterAnchor

bottomAxis :: RealAxis
bottomAxis = def
             & ticksTransform .~ V2 (V2 1 0) (V2 0 (-1)) -- flip Y axis
             & ticks. labelOffset .~ V2 (-5) 0
             & ticks . labelBaseline .~ C.HangingBaseline
             & ticks . labelAnchor .~ C.CenterAnchor

leftAxis :: RealAxis
-- leftAxis = RealAxis linearScale threeTicks calcDomain 5 10 rot90 (V2 (-5) 0) "" 12 0 C.MiddleBaseline C.RightAnchor
--            where rot90 = V2 (V2 0 (-1)) (V2 1 0)
leftAxis = def
           & ticksTransform .~ V2 (V2 0 (-1)) (V2 1 0) -- rotate CW
           & ticks . labelOffset .~ V2 (-5) 0
           & ticks . labelBaseline .~ C.MiddleBaseline
           & ticks . labelAnchor .~ C.RightAnchor
           & labelAngle .~ pi / 2
           & labelBaseline .~ C.AlphabeticBaseline

topAxis :: RealAxis
topAxis = def
          & ticks . labelOffset .~ V2 5 0
          & ticks . labelBaseline .~ C.AlphabeticBaseline
          & ticks . labelAnchor .~ C.CenterAnchor

rightAxis :: RealAxis
rightAxis = bottomAxis
            & ticksTransform .~ V2 (V2 0 1) (V2 1 0)
            & ticks . labelOffset .~ V2 5 0
            & ticks . labelBaseline .~ C.MiddleBaseline
            & ticks . labelAnchor .~ C.LeftAnchor

-- TODO account for tickLabelOffset here
-- or switch to iterative fix-point
axisSize :: RealAxis -> Double
axisSize a = a ^. labelSize + a ^. ticks . labelSize + max (_majorLength a) (_minorLength a)

data AxisParams = AxisParams {
    _axDomain :: Interval Double,
    _axScale :: Double -> Double
    -- _outputScale :: Double -> Double
    }

instance Default AxisParams where
    def = AxisParams empty id

drawAxis :: AxisParams -> RealAxis -> C.Canvas ()
drawAxis p ax = C.saveRestore $ do
    -- draw line length of plot
    C.beginPath()
    D.moveTo $ _ticksTransform ax !* V2 0 0
    D.lineTo $ _ticksTransform ax !* V2 (_axScale p . sup $ _axDomain p) 0
    -- C.textBaseline $ ax ^. ticks . labelBaseline
    -- C.textAlign $ ax ^. ticks . labelAnchor
    -- draw ticks
    mapM_ (drawTick ax) ((ax ^. ticks . ticksOver (_axDomain p)) & mapped . _1 %~ _axScale p)
    C.stroke()
    -- draw label
    let
        x = _axScale p . midpoint . _axDomain $ p
        y = ax ^. majorLength + ax ^. ticks . labelSize
    drawLabel $ (ax ^. label) & labelOffset +~ (ax ^. ticksTransform) !* (V2 x y + ax ^. labelOffset)

-- | drawTick expects pre-scaled coordinates in its second argument
drawTick :: RealAxis -> (Double, Maybe Text) -> C.Canvas ()
drawTick ax (x, l) = let
    tickLength = case l of
        Nothing -> _minorLength ax
        Just _ -> _majorLength ax
    in do
    -- tick mark
    D.moveTo $ _ticksTransform ax !* V2 x 0
    D.lineTo $ _ticksTransform ax !* V2 x tickLength
    -- tick label
    case l of
     Just txt -> do
         drawLabel $ (ax ^. ticks . label)
             & labelText .~ txt
             & labelOffset +~ _ticksTransform ax !* (V2 x tickLength ^+^ ax ^. ticks . labelOffset)
     _-> return ()
