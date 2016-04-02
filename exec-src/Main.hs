{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Game.Lib   as GLib
import           SDL.Run (ScreenInfo (..), runWithContext)

main :: IO ()
main = runWithContext
          (ScreenInfo 640 480 "Strat")
          GLib.initialState
          GLib.iterateFunc
