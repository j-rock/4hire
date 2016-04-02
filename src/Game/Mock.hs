{-# LANGUAGE RecordWildCards #-}

module Game.Mock (go) where

import qualified SDL.Run as SDL
import qualified SDL

go :: SDL.Context -> SDL.MilliDelta -> IO ()
go SDL.Context{..} _ = drawHexagon renderer


drawHexagon :: SDL.Renderer -> IO ()
drawHexagon _ = pure ()
