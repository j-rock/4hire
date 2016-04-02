module Game.Lib.Units.Instances
    ( mercenary    -- charlie
    , beretta      -- kilroy
    , barracuda    -- panther
    , flechette    -- bison
    , clybourne    -- lenet
    , sphinx       -- polar
    , souleater    -- grizzly
    , raptor       -- slagger
    , silverback   -- titan
    , zeus         -- giant
    , scythe       -- eagle
    , phantom      -- falcon
    , deathstalker -- hunter
    , halberd      -- hadrian
    , mandrake     -- octopus
    , kraken       -- atlas
    , dutchess     -- rabbit
    , destrier     -- lynx
    , sabre        -- seeker
    , falchion     -- hawkeye
    , zephyr       -- mule
    , harrier      -- pelican
    , blockade     -- trigger
    ) where

import           Game.Lib.Types.Team
import           Game.Lib.Types.Terrain
import           Game.Lib.Types.Unit

import           Control.Lens

attackBoth :: Integer -> Integer -> UnitType -> Maybe Integer
attackBoth air _    Air  = Just air
attackBoth _   land Land = Just land

attackAir :: Integer -> UnitType -> Maybe Integer
attackAir air Air  = Just air
attackAir _   Land = Nothing

attackLand :: Integer -> UnitType -> Maybe Integer
attackLand _    Air  = Nothing
attackLand land Land = Just land

defAttack :: Integer -> Integer -> Attacks
defAttack land air = Attack (attackBoth air land) (Just Direct)

pacifist :: Attacks
pacifist = Attack (const Nothing) Nothing

defMobility :: Integer -> Mobility
defMobility shiftDist = Shift shiftDist spd
    where spd Road   = Fast
          spd Bridge = Fast
          spd _      = Slow

moveOnce :: Mobility
moveOnce = PlaceOnce False

canCapture :: Capture
canCapture = Capture True

can'tCapture :: Capture
can'tCapture = Capture False

mkTurn :: TurnType -> Turn
mkTurn tu = Turn tu 0 False

defTurn :: Turn
defTurn = mkTurn MoveThenAttack

moveOrAttackTurn :: Turn
moveOrAttackTurn = mkTurn EitherMoveOrAttack

moveAfterAttack :: Turn
moveAfterAttack = mkTurn MoveAttackMoveAgain

defExperience :: Experience
defExperience = Experience (Level 1) evade acc
   where evade = const 0.15
         acc   = const 0.15

carry      = True
can'tCarry = False

defTroopers :: Defense -> Troopers Unit
defTroopers d = Troopers (replicate 8 troop) CantCarry
    where troop = Trooper (d^.defval) Nothing

type UnitT = Team -> Unit


-------------------------------------
-- Infantry
-------------------------------------

infantry :: Defense -> Attacks -> Mobility -> String -> UnitT
infantry d a m = Unit d a m canCapture defTurn defExperience (defTroopers d) Small Land


mercenary :: UnitT
mercenary =
    infantry
      (Defense 4)
      (defAttack 10 10)
      (defMobility 3)
      "Mercenary"

beretta :: UnitT
beretta =
    infantry
      (Defense 10)
      (defAttack 40 10)
      (defMobility 2)
      "Beretta"

barracuda :: UnitT
barracuda =
    infantry
      (Defense 8)
      (defAttack 10 10)
      (defMobility 9 & speeds .~ spd)
      "Barracuda"
  where spd Road     = Fast
        spd Bridge   = Fast
        spd Cracked  = Blocked
        spd Mountain = Blocked
        spd _        = Slow

-------------------------------------
-- Ground Units
-------------------------------------

groundAttack :: Integer -> Attacks
groundAttack land = Attack (attackLand land) (Just Direct)

groundUnit :: Defense -> Attacks -> Mobility -> String -> UnitT
groundUnit d a m = Unit d a m can'tCapture defTurn defExperience (defTroopers d) Medium Land


flechette :: UnitT
flechette =
    groundUnit
      (Defense 40)
      (groundAttack 50)
      (defMobility 6)
      "Flechette"

clybourne :: UnitT
clybourne =
    groundUnit
      (Defense 30)
      (groundAttack 45)
      (defMobility 5)
      "Clybourne"

sphinx :: UnitT
sphinx =
    groundUnit
      (Defense 60)
      (groundAttack 60)
      (defMobility 4)
      "Sphinx"

souleater :: UnitT
souleater =
    groundUnit
      (Defense 50)
      (groundAttack 70)
      (defMobility 4)
      "Souleater"

raptor :: UnitT
raptor =
    groundUnit
      (Defense 50)
      (groundAttack 50)
      (defMobility 7)
      "Raptor"

silverback :: UnitT
silverback =
    groundUnit
      (Defense 50)
      (groundAttack 60)
      (defMobility 5)
      "Silverback"

zeus :: UnitT
zeus team =
    groundUnit
      (Defense 80)
      (defAttack 90 40)
      (defMobility 2)
      "Zeus"
      team
      & usize .~ Large

-------------------------------------
-- Aircraft
-------------------------------------

aircraftAttack :: Integer -> Attacks
aircraftAttack air = Attack (attackAir air) (Just Direct)

airMobility :: Integer -> Mobility
airMobility i = Shift i (const Fast)

aircraft :: Defense -> Attacks -> Mobility -> String -> UnitT
aircraft d a m = Unit d a m can'tCapture defTurn defExperience (defTroopers d) Medium Air

scythe :: UnitT
scythe =
    aircraft
      (Defense 30)
      (defAttack 70 20)
      (airMobility 10)
      "Scythe"

phantom :: UnitT
phantom =
    aircraft
      (Defense 30)
      (aircraftAttack 90)
      (airMobility 12)
      "Phantom"

deathstalker :: UnitT
deathstalker =
    aircraft
      (Defense 50)
      (defAttack 70 70)
      (airMobility 11)
      "Deathstalker"

-------------------------------------
-- Artillery
-------------------------------------

artilleryAttack :: Integer -> Integer -> Attacks
artilleryAttack range land = Attack (attackLand land) (Just $ Indirect 2 range)

artillery :: Defense -> Attacks -> Mobility -> String -> UnitT
artillery d a m = Unit d a m can'tCapture moveOrAttackTurn defExperience (defTroopers d) Medium Land

halberd :: UnitT
halberd =
    artillery
      (Defense 30)
      (artilleryAttack 5 45)
      (defMobility 4)
      "Halberd"

mandrake :: UnitT
mandrake =
    artillery
      (Defense 30)
      (artilleryAttack 4 60)
      (defMobility 4)
      "Mandrake"

kraken :: UnitT
kraken =
    artillery
      (Defense 20)
      (artilleryAttack 6 90)
      moveOnce
      "Kraken"

-------------------------------------
-- Missile Buggies
-------------------------------------

buggie :: Defense -> Attacks -> Mobility -> String -> UnitT
buggie d a m = Unit d a m can'tCapture moveAfterAttack defExperience (defTroopers d) Small Land

dutchess :: UnitT
dutchess =
    buggie
      (Defense 20)
      (defAttack 70 10)
      (defMobility 8)
      "Dutchess"

destrier :: UnitT
destrier =
    buggie
      (Defense 20)
      (defAttack 40 10)
      (defMobility 6)
      "Destrier"

-------------------------------------
-- Anti-Air
-------------------------------------

antiAir :: Defense -> Attacks -> Mobility -> String -> UnitT
antiAir d a m = Unit d a m can'tCapture defTurn defExperience (defTroopers d) Medium Land

sabre :: UnitT
sabre =
    antiAir
      (Defense 30)
      (defAttack 30 65)
      (defMobility 6)
      "Sabre"

falchion :: UnitT
falchion =
    antiAir
      (Defense 30)
      (Attack (attackAir 85) (Just $ Indirect 2 5))
      (defMobility 5)
      "Falchion"

-------------------------------------
-- Transport
-------------------------------------

transportCarry = troops . tCarry

transport :: Defense -> Attacks -> Mobility -> String -> UnitT
transport d a m = Unit d a m can'tCapture defTurn defExperience (defTroopers d) ExtraLarge Air

zephyr :: UnitT
zephyr team =
    transport
      (Defense 10)
      (defAttack 10 10)
      (defMobility 6)
      "Zephyr"
      team
      & transportCarry .~ Carry Medium

harrier :: UnitT
harrier team =
    transport
      (Defense 10)
      pacifist
      (defMobility 9)
      "Harrier"
      team
      & transportCarry .~ Carry Large

-------------------------------------
-- Mines
-------------------------------------

mine :: Defense -> Attacks -> Mobility -> String -> UnitT
mine d a m = Unit d a m can'tCapture defTurn defExperience (defTroopers d) Small Land

blockade :: UnitT
blockade =
    mine
      (Defense 80)
      pacifist
      moveOnce
      "Blockade"
