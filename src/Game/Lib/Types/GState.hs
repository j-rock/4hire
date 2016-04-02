module Game.Lib.Types.GState where

import qualified SDL.Run        as SDL

data GState = GState
            deriving (Show)

iterateGState :: GState -> SDL.IterateStateFunc IO GState
iterateGState g sdl dt = \x ->

-- iterateFunc sdl dt x = do Mock.go sdl dt
--                           pure (x, SDL.StillPlaying)
