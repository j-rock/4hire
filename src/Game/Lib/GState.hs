module Game.Lib.GState
    ( module Game.Lib.GState.Battle
    , module Game.Lib.GState.MainMenu
    , module Game.Lib.GState.Splash
    , initialState
    ) where

import           Game.Lib.Types.GState
import           Game.Lib.GState.Battle
import           Game.Lib.GState.MainMenu
import           Game.Lib.GState.Splash

initialState :: GState
initialState = defaultSplash
