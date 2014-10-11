{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sluice.Canvas where

import Graphics.Blank

import Data.Text  (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Monoid
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Linear
import Linear.Affine
import Data.Default.Class
import Control.Lens hiding (over)

data Shape = Rect

data Marker = Marker {
    _shape :: Shape,
    _size :: Double,
    _fillColor :: AlphaColour Double,
    _lineColor :: AlphaColour Double
    }

makeLenses ''Marker

instance Default Marker where
    def = Marker Rect 10 (opaque blue) (opaque black)

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

draw :: Marker -> Point V2 Double -> Canvas ()
draw m p = do
    let s = (m ^. size)
    case m ^. shape of
      Rect -> saveRestore $ do
          translate(p ^. _x, p ^. _y)
          fillRect(-s/2, -s/2, s, s)
          strokeRect(-s/2, -s/2, s, s)
