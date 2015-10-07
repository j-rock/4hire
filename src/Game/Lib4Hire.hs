module Game.Lib4Hire
    ( module Game.Lib4Hire.Types.GState
    , iterateFunc
    ) where

import Game.Lib4Hire.Types.GState
import qualified SDL.Extras as SDL

iterateFunc :: SDL.IterateStateFunc IO GState
iterateFunc _ _ x = pure (x, SDL.StillPlaying)
