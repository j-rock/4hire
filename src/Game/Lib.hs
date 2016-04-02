module Game.Lib
    ( module Game.Lib.Types

    , module Game.Lib.Units
    , module Game.Lib.GState

    , iterateFunc
    ) where

import           Game.Lib.GState
import           Game.Lib.Types
import           Game.Lib.Units

import qualified Game.Mock      as Mock


iterateFunc :: SDL.IterateStateFunc IO GState
iterateFunc = iterateGState Mock.state
