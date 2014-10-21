{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Blank
import Sluice
import Sluice.Text
import Sluice.Axis

import Control.Monad
import System.Random
import Control.Lens

main :: IO ()
main = do
    xVals <- replicateM 100 $ randomRIO (0,100)
    yVals <- replicateM 100 $ randomRIO (0,1000)
    blankCanvas 3000 $ \context ->
        send context . plot $ scatter
          & xs .~ xVals
          & ys .~ yVals
          & xAxis . labelText .~ "Randomly Distributed over [0,100]"
          & yAxis . labelText .~ "Randomly Distributed over [0,1000]"
