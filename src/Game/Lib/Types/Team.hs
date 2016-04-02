{-# LANGUAGE TemplateHaskell #-}

module Game.Lib.Types.Team where

import           Control.Lens


data Team = Hero
          | Villain
          deriving (Eq, Ord, Show, Enum, Bounded)
makePrisms ''Team
