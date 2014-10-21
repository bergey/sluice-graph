{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sluice.Canvas where

import Graphics.Blank as C
import Sluice.Util

import Data.Text  (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Monoid
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Linear
import Data.Default.Class
import Control.Lens hiding (over)

data Shape = Rect | Circle

data Marker = Marker {
    _shape :: Shape,
    _size :: Double,
    _fillColor :: AlphaColour Double,
    _lineColor :: AlphaColour Double
    }

makeLenses ''Marker

instance Default Marker where
    def = Marker Circle 5 (opaque blue) (opaque black)

colorText :: RealFloat n => AlphaColour n -> Text
colorText ac = toStrict . toLazyText $
               "rgba(" <> f r <> c <> f g <> c <> f b <> c <> f a <> rParen
  where
    a = alphaChannel ac
    c = singleton ','
    (RGB r g b) = toSRGB $ ac `over` black
    f x = decimal (round $ x * 255 :: Int)
    rParen = singleton ')'

withStyle :: Marker -> Canvas a -> Canvas a
withStyle m c = saveRestore $ do
    fillStyle . colorText . view fillColor $ m
    strokeStyle . colorText . view lineColor $ m
    c

drawMark :: Marker -> V2 Double -> Canvas ()
drawMark m p = do
    let s = (m ^. size)
    case m ^. shape of
      Rect -> do
          fillRect(p ^. _x -s/2, p^. _y - s/2, s, s)
          strokeRect(p ^. _x -s/2,p ^. _y -s/2, s, s)
      Circle -> do
          beginPath()
          arc(p ^. _x, p ^. _y,s/2,0,2*pi,False)
          stroke()
          fill()

drawMarks :: Marker -> [V2 Double] -> Canvas ()
drawMarks m ps = withStyle m $ mapM_ (drawMark m) ps

invertY :: Canvas () -> Canvas ()
invertY c = saveRestore $ do
    me <- myCanvasContext
    translate(0, height me)
    scale(1,-1)
    c

-- | drawText compensates for the flipped coordinate system of Canvas
drawText :: Text -> V2 Double -> Canvas ()
drawText txt (V2 x y) = saveRestore $ do
    translate(x,y)
    scale(1,-1)
    fillText(txt,0,0)

-- basic wrappers to use types from Linear

moveTo :: V2 Double -> Canvas ()
moveTo (V2 x y) = C.moveTo (x,y)

lineTo :: V2 Double -> Canvas ()
lineTo (V2 x y) = C.lineTo (x,y)

-- | set the font size, resetting other font parameters in the process.
fontSize :: Double -> Canvas ()
fontSize s = C.font $ text s <> "pt"
