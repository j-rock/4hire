{-# LANGUAGE TemplateHaskell #-}

module Game.Lib.Types.Terrain where

import Control.Lens

data Terrain = Road
             | Bridge
             | Hill
             | Cracked
             | Mountain
             | Valley
             deriving (Eq, Ord, Show, Bounded, Enum)
makePrisms ''Terrain
