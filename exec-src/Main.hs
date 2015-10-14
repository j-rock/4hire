{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Game.Lib   as GLib
import           SDL.Extras (ScreenInfo (..), runWithSDLContext)

main :: IO ()
main = runWithSDLContext
          (ScreenInfo 640 480 "Strat")
          GLib.initialState
          GLib.iterateFunc
