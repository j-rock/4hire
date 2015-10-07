{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Game.Lib4Hire as L4Hire
import           SDL.Extras    (ScreenInfo (..), runWithSDLContext)

main :: IO ()
main = runWithSDLContext
          (ScreenInfo 640 480 "4Hire")
          L4Hire.initialState
          L4Hire.iterateFunc
