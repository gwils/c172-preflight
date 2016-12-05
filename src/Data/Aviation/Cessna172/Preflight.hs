{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight where

import Prelude
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine
import Plots
import Control.Lens

data Arm =
  Arm {
    _armmeasure ::
      Int -- inches
  , _armrange ::
      Maybe (Int, Int)
  , _name ::
      Maybe String
  } deriving (Eq, Ord, Show)

makeClassy ''Arm

armnorange :: 
  Int
  -> Maybe String
  -> Arm
armnorange n m =
  Arm n Nothing m


data AircraftArm =
  AircraftArm {
    _aeroplane ::
      Arm
  , _frontseat ::
      Arm
  , _fuel ::
      Arm
  , _rearseat ::
      Arm
  , _baggagea ::
      Arm
  , _baggageb ::
      Arm
  }
  deriving (Eq, Ord, Show)

c172arm ::
  Arm -- aeroplane arm
  -> AircraftArm
c172arm a =
  AircraftArm
    a
    (Arm 37 (Just (34, 46)) (Just "front seat"))
    (armnorange 48 (Just "fuel"))
    (armnorange 73 (Just "rear seat"))
    (Arm 95 (Just (82, 108)) (Just "baggage A"))
    (Arm 123 (Just (108, 142)) (Just "baggage B"))

-- baggage "A" maximum 120lb
-- baggage "B" maximum 50lb
-- maximum overall baggage 120lb

data AircraftWeight =
  AircraftWeight {
    _bew ::
      Double
  , _frontseatweight ::
      Double -- pounds
  , _fuelweight ::
      Double -- gallons
  , _rearseatweight ::
      Double
  , _baggageaweight ::
      Double
  , _baggagebweight ::
      Double
  }
  deriving (Eq, Ord, Show)

data MaximumWeight =
  MaximumWeight {
    baggagea ::
      Int
  , baggageb ::
      Int
  , totalbaggage ::
      Int
  , fuel ::
      Int
  , mrw ::
      Int
  , mtow ::
      Int
  }
  deriving (Eq, Ord, Show)
    
c172RMaximumWeight ::
  MaximumWeight
c172RMaximumWeight =
  MaximumWeight
    120
    50
    120
    336
    2558
    2550

sample ::
  AircraftWeight
sample =
  AircraftWeight
    1691.6
    363.763
    30
    176.37
    22.0462
    2.20462

-- https://hackage.haskell.org/package/hgeometry-0.5.0.0/docs/Data-Geometry-Polygon.html

-- AircraftArm -> MaximumWeight -> AircraftWeight -> CGEnvelope -> Report

{-
Weight (lbs): 1663.2
Arm (in): 40.719
Moment (lb-in): 67724
Basic Empty Weight

    Weight (lbs): 1691.6
    Arm (in): 40.600
    Moment (lb-in): 68679
-}

----

---- TODR/LDR

data TakeOffDistanceAltitude =
  TakeOffDistanceAltitude {
    _temp00 ::
      Int
  , _temp10 ::
      Int
  , _temp20 ::
      Int
  , _temp30 ::
      Int
  , _temp40 ::
      Int
  }
  deriving (Eq, Ord, Show)

makeClassy ''TakeOffDistanceAltitude

data TakeOffDistance =
  TakeOffDistance {
    _pa0000 ::
      TakeOffDistanceAltitude
  , _pa1000 ::
      TakeOffDistanceAltitude
  , _pa2000 ::
      TakeOffDistanceAltitude
  , _pa3000 ::
      TakeOffDistanceAltitude
  , _pa4000 ::
      TakeOffDistanceAltitude
  , _pa5000 ::
      TakeOffDistanceAltitude
  , _pa6000 ::
      TakeOffDistanceAltitude
  , _pa7000 ::
      TakeOffDistanceAltitude
  , _pa8000 ::
      TakeOffDistanceAltitude
  }
  deriving (Eq, Ord, Show)

makeClassy ''TakeOffDistance

c172s_2550lbs_groundroll_takeoff ::
  TakeOffDistance
c172s_2550lbs_groundroll_takeoff =
  TakeOffDistance
    (
      TakeOffDistanceAltitude
        860
        925
        995
        1070
        1150
    )
    (
      TakeOffDistanceAltitude
        940
        1010
        1090
        1170
        1260
    )
    (
      TakeOffDistanceAltitude
        1025
        1110
        1195
        1285
        1380
    )
    (
      TakeOffDistanceAltitude
        1125
        1215
        1310
        1410
        1515
    )
    (
      TakeOffDistanceAltitude
        1235
        1335
        1440
        1550
        1660
    )
    (
      TakeOffDistanceAltitude
        1355
        1465
        1585
        1705
        1825
    )
    (
      TakeOffDistanceAltitude
        1495
        1615
        1745
        1875
        2010
    )
    (
      TakeOffDistanceAltitude
        1645
        1785
        1920
        2065
        2215
    )
    (
      TakeOffDistanceAltitude
        1820
        1970
        2120
        2280
        2450
    )

newtype PressureAltitude =
  PressureAltitude
    Int
  deriving (Eq, Ord, Show)

class AsPressureAltitude r0 where
  _PressureAltitude ::
    Prism' r0 PressureAltitude

instance AsPressureAltitude PressureAltitude where
  _PressureAltitude =
    id

instance AsPressureAltitude Int where
  _PressureAltitude =
    prism'
      (\(PressureAltitude n) -> n)
      (\n -> if n < 0 || n > 8000
               then
                 Nothing
              else
                 Just (PressureAltitude n))

newtype Temperature =
  Temperature
    Int
  deriving (Eq, Ord, Show)

class AsTemperature r0 where
  _Temperature ::
    Prism' r0 Temperature

instance AsTemperature Temperature where
  _Temperature =
    id

instance AsTemperature Int where
  _Temperature =
    prism'
      (\(Temperature n) -> n)
      (\n -> if n < 0 || n > 40
               then
                 Nothing
              else
                 Just (Temperature n))


intervalsPressureAltitude ::
  PressureAltitude
  -> TakeOffDistance
  -> (TakeOffDistanceAltitude, TakeOffDistanceAltitude, Int)
intervalsPressureAltitude (PressureAltitude n) (TakeOffDistance _0 _1 _2 _3 _4 _5 _6 _7 _8) =
  let (d, m) =
        n `divMod` 1000
      (r, s) =
        case d of
          0 ->
            (_0, _1)
          1 ->
            (_1, _2)
          2 ->
            (_2, _3)
          3 ->
            (_3, _4)
          4 ->
            (_4, _5)
          5 ->
            (_5, _6)
          6 ->
            (_6, _7)
          7 ->
            (_7, _8)
          8 ->
            (_7, _8)
          _ ->
            error "TODO"
  in  (r, s, m)

intervalsTemperature ::
  Temperature
  -> TakeOffDistanceAltitude
  -> (Int, Int, Int)
intervalsTemperature (Temperature n) (TakeOffDistanceAltitude _0 _1 _2 _3 _4) =
  let (d, m) =
        n `divMod` 10
      (r, s) =
        case d of
          0 ->
            (_0, _1)
          1 ->
            (_1, _2)
          2 ->
            (_2, _3)
          3 ->
            (_3, _4)
          4 ->
            (_3, _4)
          _ -> 
            error "TODO"          
  in  (r, s, m)


{-
.---.-----.------.-----.
|   |  a  |  b   |  c  |
:---+-----+------+-----:
| d | k_1 |      | k_2 |
:---+-----+------+-----:
| e |     | todr |     |
:---+-----+------+-----:
| f | k_3 |      | k_4 |
'---'-----'------'-----'
-}

-- todr (PressureAltitude 100) (Temperature 10) c172s_2550lbs_groundroll_takeoff

-- |
--
-- >>> todr (PressureAltitude 3500) (Temperature 25) c172s_2550lbs_groundroll_takeoff
-- 1427.5
--
-- >>> todr (PressureAltitude 3000) (Temperature 20) c172s_2550lbs_groundroll_takeoff
-- 1310.0
--
-- >>> todr (PressureAltitude 3000) (Temperature 25) c172s_2550lbs_groundroll_takeoff
-- 1360.0
--
-- >>> todr (PressureAltitude 3500) (Temperature 20) c172s_2550lbs_groundroll_takeoff
-- 1375.0
--
-- >>> todr (PressureAltitude 3250) (Temperature 27) c172s_2550lbs_groundroll_takeoff
-- 1414.25
--
-- >>> todr (PressureAltitude 5130) (Temperature 14) c172s_2550lbs_groundroll_takeoff
-- 1533.02
todr ::
  PressureAltitude
  -> Temperature
  -> TakeOffDistance
  -> Double
todr e b chart =
  let (x,  y,  q) = intervalsPressureAltitude e chart
      (x0, y0, r) = intervalsTemperature b x
      (x1, y1, _) = intervalsTemperature b y
      normpa g h  = fromIntegral ((h - g) * q) / 1000 + fromIntegral g
      normtp g h  = (h - g) * fromIntegral r / 10 + g
  in  normtp (normpa x0 x1) (normpa y0 y1)

----


---- Centre of Gravity

data COGResult = Utility | Normal | Outside

locate ::
  Double
  -> Double
  -> COGResult
locate =
  undefined

-- |
--
-- >>> _inner 65
-- 1600
-- >>> _inner 71
-- 1750
_inner :: Int -> Maybe Int
_inner x = 
  if x < 89
    then Just (25*x-25)
    else Nothing

---- 
---- Moment Envelope


_polygon1 = [(120.5, 2550), (71, 1500), (52.5,1500), (68,1950), (104.5, 2550)]
_polygon2 = [(61, 1500), (89, 2200), (82.5, 2200)]


myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  linePlot' _polygon1
  linePlot' _polygon2

main :: IO ()
main = r2AxisMain myaxis
