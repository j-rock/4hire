module Game.Lib
    ( module Game.Lib.Types.GState
    , iterateFunc
    ) where

import           Game.Lib.Types.GState
import qualified SDL.Extras            as SDL

iterateFunc :: SDL.IterateStateFunc IO GState
iterateFunc _ _ x = pure (x, SDL.StillPlaying)
