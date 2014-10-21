{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sluice.Text where

import qualified Graphics.Blank as C
import Sluice.Canvas

import Data.Default.Class
import Data.Text (Text)
import Control.Lens
import Linear

data Label = Label {
    _labelText :: Text,
    _labelSize :: Double,
    _labelAngle :: Double,
    _labelOffset :: V2 Double,
    _labelBaseline :: C.TextBaselineAlignment,
    _labelAnchor :: C.TextAnchorAlignment
    }

-- TODO rename HasLabel to Labeled
makeClassy ''Label

instance Default Label where
    def = Label "" 12 0 zero def def

drawLabel :: Label -> C.Canvas ()
drawLabel l = C.saveRestore $ do
    let V2 x y = l ^. labelOffset
    C.translate(x,y)
    C.scale(1,-1)
    fontSize $ l ^. labelSize
    C.rotate $ (-1) * l ^. labelAngle
    C.textBaseline $ l ^. labelBaseline
    C.textAlign $ l ^. labelAnchor
    C.fillText(l ^. labelText,0,0)
