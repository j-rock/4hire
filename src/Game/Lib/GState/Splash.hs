module Game.Lib.GState.Splash where

import Game.Lib.Types.GState

data Splash = Splash ()
            deriving (Show)

defaultSplash :: GState
defaultSplash = GState
