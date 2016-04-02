{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Lib.Types.Unit where

import           Game.Lib.Types.Team
import           Game.Lib.Types.Terrain

import           Control.Lens        hiding (Level)

newtype Defense = Defense {_defval :: Integer} deriving (Eq, Ord, Show)
makeLenses ''Defense

data UnitType = Air | Land deriving (Eq, Ord, Show, Enum, Bounded)
makePrisms ''UnitType

data AttackRange = Direct
                 | Indirect {
                     _closeDist :: Integer
                   , _farDist   :: Integer
                   }
                 deriving (Eq, Ord, Show)
makeLenses ''AttackRange
makePrisms ''AttackRange

data Attacks = Attack {
                 _atk      :: UnitType -> Maybe Integer
               , _atkRange :: Maybe AttackRange
               }
makeLenses ''Attacks

instance Show Attacks where
    show a = concat [ "Attacks {"
                    , show (tk Air, tk Land)
                    , ","
                    , show (a ^. atkRange)
                    , "}"
                    ]
        where tk = a ^. atk

data Speed = Fast
           | Slow
           | Blocked
           deriving (Eq, Ord, Show, Bounded, Enum)
makePrisms ''Speed

data Mobility = PlaceOnce {
                  _hasBeenPlaced :: Bool
                }
              | Shift {
                  _maxDist :: Integer
                , _speeds :: Terrain -> Speed
                }
makeLenses ''Mobility
makePrisms ''Mobility

instance Show Mobility where
    show PlaceOnce{..} = "PlaceOnce {" ++ show _hasBeenPlaced ++ "}"
    show Shift{..} = "Shift { _maxDist = " ++ show _maxDist ++ "}"

newtype Capture = Capture {_canCap :: Bool} deriving (Eq, Ord, Show)
makeLenses ''Capture

data TurnType = MoveThenAttack
              | EitherMoveOrAttack
              | MoveAttackMoveAgain
              deriving (Eq, Ord, Show, Enum, Bounded)
makePrisms ''TurnType

data Turn = Turn {
              _turnt       :: TurnType
            , _movedAmt    :: Integer
            , _hasAttacked :: Bool
            } deriving (Eq, Ord, Show)
makeLenses ''Turn


newtype Level = Level {_lvl :: Integer} deriving (Eq, Ord, Show)
makeLenses ''Level

data Experience = Experience {
                    _curlvl    :: Level
                  , _evasion   :: Level -> Rational
                  , _precision :: Level -> Rational
                  }
makeLenses ''Experience

instance Show Experience where
    show e = "Experience {" ++ show (e^.curlvl) ++ "}"

data Trooper a = Trooper {
                   _health  :: Integer
                 , _package :: Maybe a
                 } deriving (Eq, Ord, Show)
makeLenses ''Trooper

data Size = Small
          | Medium
          | Large
          | ExtraLarge
          deriving (Eq, Ord, Show, Enum, Bounded)
makePrisms ''Size

data Carry = CantCarry
           | Carry {_maxUnitSize :: Size}
           deriving (Eq, Ord, Show)
makeLenses ''Carry
makePrisms ''Carry


data Troopers a = Troopers {
                    _trps   :: [Trooper a]
                  , _tCarry :: Carry
                  } deriving (Eq, Ord, Show)
makeLenses ''Troopers

data Unit = Unit {
              _defen  :: Defense
            , _attack :: Attacks
            , _shift  :: Mobility
            , _ucapt  :: Capture
            , _uturn  :: Turn
            , _xp     :: Experience
            , _troops :: Troopers Unit
            , _usize  :: Size
            , _utype  :: UnitType
            , _uname  :: String
            , _uteam  :: Team
            } deriving (Show)
makeLenses ''Unit
