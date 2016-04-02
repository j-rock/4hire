{-# LANGUAGE TemplateHaskell #-}

module Game.Lib.GState.Battle where

import Game.Lib.Types.Building
import Game.Lib.Types.Unit
import Game.Lib.Types.Team
import Game.Lib.Types.Terrain

import Control.Lens

type HexMap a = ()

data Cell = Cell {
              _terrain   :: Terrain
            , _height    :: Integer
            , _cbuilding :: Maybe Building
            , _cunit     :: Maybe Unit
            } deriving (Show)
makeLenses ''Cell

data Battle = Battle {
                _cells     :: HexMap Cell
              , _curPlayer :: Team
              } deriving (Show)
makeLenses ''Battle
