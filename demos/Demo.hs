{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Blank
import Sluice

import Control.Monad
import System.Random

main :: IO ()
main = do
    xVals <- replicateM 100 $ randomRIO (0,100)
    yVals <- replicateM 100 $ randomRIO (0,1000)
    blankCanvas 3000 $ \context ->
        send context $ plot scatter (xVals, yVals)
