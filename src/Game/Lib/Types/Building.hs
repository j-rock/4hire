{-# LANGUAGE TemplateHaskell #-}

module Game.Lib.Types.Building where

import           Game.Lib.Types.Team
import           Game.Lib.Types.Unit

import           Control.Lens


data Structure = Prison
               | Factory {
                   _units :: [Maybe Unit]
                 }
               deriving (Show)
makeLenses ''Structure
makePrisms ''Structure

data Building = Building {
                  _bteam   :: Team
                , _bstruct :: Structure
                } deriving (Show)
makeLenses ''Building
