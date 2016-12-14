{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aviation.Cessna172.Preflight where

import Prelude
import Data.Foldable(toList)
import Data.Set(Set)
import qualified Data.Set as Set(singleton, fromList, empty, map)
import Diagrams.Prelude(V2)
import Diagrams.Backend.Rasterific.CmdLine(B)
import Plots(Axis, r2Axis, r2AxisMain, linePlot')
import Control.Lens(Prism', _Wrapped, makeWrapped, makeClassy, prism', (&~), (^.), (&))
import Data.CircularSeq(CSeq)
import Data.Geometry.Boundary(PointLocationResult)
import Data.Geometry.Line.Internal(sqDistanceToArg, supportingLine)
import Data.Geometry.Point(Point(Point), point2)
import Data.Geometry.Polygon(SimplePolygon, inPolygon, fromPoints, outerBoundaryEdges)
import Data.Geometry.Vector(Arity, Vector(Vector))
import qualified Data.Vector.Fixed as FV(length)
import Data.Ext(ext)

{-

Set a -- all arms
(a -> Name) -- names
(a -> Capacity) -- weights/volumes
(a -> Arm) -- arms
[Set a, Capacity] -- limits

-}

data Allarmtypes a =
  Allarmtypes
    (Set a)
  deriving (Eq, Ord, Show)

makeWrapped ''Allarmtypes

oneAllarmtypes ::
  a
  -> Allarmtypes a
oneAllarmtypes =
  Allarmtypes . Set.singleton

mapAllarmtypes ::
  Ord b =>
  (a -> b)
  -> Allarmtypes a
  -> Allarmtypes b
mapAllarmtypes f (Allarmtypes x) =
  Allarmtypes (Set.map f x)

instance Ord a => Monoid (Allarmtypes a) where
  mempty =
    Allarmtypes Set.empty
  Allarmtypes a `mappend` Allarmtypes b =
    Allarmtypes (a `mappend` b)

newtype Armname =
  Armname
    String -- todo
  deriving (Eq, Ord, Show)

makeWrapped ''Armname

newtype GetArmname a =
  GetArmname
    (a -> Armname)

makeWrapped ''GetArmname

data Capacity =
  Capacity
    Rational -- todo, assume lbs for now
  deriving (Eq, Ord, Show)

makeClassy ''Capacity

newtype GetCapacity a =
  GetCapacity
    (a -> Capacity)

makeWrapped ''GetCapacity

data Arm =
  Arm {
    _armmeasure ::
      Rational -- inches
  , _armrange ::
      Maybe (Rational, Rational)
  }
  deriving (Eq, Ord, Show)

makeClassy ''Arm

newtype GetArm a =
  GetArm
    (a -> Arm)

makeWrapped ''GetArm

data Limit a =
  Limit
    (Set a)
    Capacity
  deriving (Eq, Ord, Show)

makeClassy ''Limit

onelimit ::
  a
  -> Capacity
  -> Limit a
onelimit =
  Limit . Set.singleton

mapLimit ::
  Ord b =>
  (a -> b)
  -> Limit a
  -> Limit b
mapLimit f (Limit x z) =
  Limit (Set.map f x) z

newtype Limits a =
  Limits
    [Limit a]
  deriving (Eq, Ord, Show)

makeWrapped ''Limits

instance Monoid (Limits a) where
  mempty =
    Limits mempty
  Limits a `mappend` Limits b =
    Limits (a `mappend` b)

onelimits ::
  a
  -> Capacity
  -> Limits a
onelimits a c =
  Limits [onelimit a c]

armnorange :: 
  Rational
  -> Arm
armnorange n =
  Arm n Nothing

mapLimits ::
  Ord b =>
  (a -> b)
  -> Limits a
  -> Limits b
mapLimits f (Limits x) =
  Limits (map (mapLimit f) x)

----

data C172KnownArmType =
  FrontSeat
  | RearSeat
  | Fuel
  | BaggageA
  | BaggageB
  deriving (Eq, Ord, Show)

allarmtypesC172KnownArmType ::
  Allarmtypes C172KnownArmType
allarmtypesC172KnownArmType =
  Allarmtypes (Set.fromList [FrontSeat, RearSeat, Fuel, BaggageA, BaggageB])

getarmnameC172KnownArmType ::
  GetArmname C172KnownArmType
getarmnameC172KnownArmType =
  GetArmname (
      \a -> Armname (
              case a of
                FrontSeat -> 
                  "front seat"
                RearSeat -> 
                  "rear seat"
                Fuel -> 
                  "fuel"
                BaggageA -> 
                  "baggage A"
                BaggageB -> 
                  "baggage B"
            )
    )


getarmC172KnownArmType ::
  GetArm C172KnownArmType
getarmC172KnownArmType =
  GetArm (
      \a -> case a of
              FrontSeat ->
                Arm 37 (Just (34, 46))
              RearSeat ->
                armnorange 73
              Fuel ->
                armnorange 48
              BaggageA ->
                Arm 95 (Just (82, 1808))
              BaggageB ->
                Arm 123 (Just (108, 142))
          
    )

limitsC172KnownArmType ::
  Limits C172KnownArmType
limitsC172KnownArmType =
  Limits
    [
      Limit (Set.singleton BaggageA) (Capacity 120)
    , Limit (Set.singleton BaggageB) (Capacity 50)
    , Limit (Set.fromList [BaggageA, BaggageB]) (Capacity 120)
    , Limit (Set.singleton Fuel) (Capacity 336)
    ]

----

data C172ArmType =
  Aircraft
  | KnownC172ArmType C172KnownArmType
  deriving (Eq, Ord, Show)

allarmtypesC172ArmType ::
  Allarmtypes C172ArmType
allarmtypesC172ArmType =
  oneAllarmtypes Aircraft `mappend` mapAllarmtypes KnownC172ArmType allarmtypesC172KnownArmType


getarmnameC172ArmType ::
  GetArmname C172ArmType
getarmnameC172ArmType =
  GetArmname (
    \a ->
      case a of
        KnownC172ArmType t ->
          t & getarmnameC172KnownArmType ^. _Wrapped
        Aircraft ->
          Armname "aircraft"
  )

getarmC172ArmType ::
  Arm -- BEW arm
  -> GetArm C172ArmType
getarmC172ArmType x =
  GetArm (
      \a -> case a of
              KnownC172ArmType t ->
                t & getarmC172KnownArmType ^. _Wrapped
              Aircraft ->
                x
    )

limitsC172ArmType ::
  Limits C172ArmType
limitsC172ArmType =
  onelimits Aircraft (Capacity 2550) `mappend` mapLimits KnownC172ArmType limitsC172KnownArmType

----

vhlsecapacityArmType ::
  GetCapacity C172KnownArmType
  -> GetCapacity C172ArmType
vhlsecapacityArmType k =
  GetCapacity (
      \a -> case a of
              KnownC172ArmType t ->
                t & k ^. _Wrapped
              Aircraft ->
                Capacity 1691.6
    )
  
vhlsearmArmType ::
  GetArm C172ArmType
vhlsearmArmType =
  getarmC172ArmType (armnorange 40.600)

----


-- test data
getcapacityC172KnownArmType ::
  GetCapacity C172KnownArmType
getcapacityC172KnownArmType =
  GetCapacity (
      \a -> Capacity (
              case a of
                FrontSeat -> 
                  363.763
                RearSeat -> 
                  176.37
                Fuel -> 
                  180
                BaggageA -> 
                  22.0462
                BaggageB -> 
                  2.20462
            )
    )

-- test data
getcapacityC172ArmType ::
  Capacity -- BEW
  -> GetCapacity C172ArmType
getcapacityC172ArmType x =
  GetCapacity (
      \a -> case a of
              KnownC172ArmType t ->
                t & getcapacityC172KnownArmType ^. _Wrapped
              Aircraft ->
                x
    )

----

totalCapacityWith  ::
  Num a =>
  (t -> Rational -> a)
  -> Allarmtypes t
  -> GetCapacity t
  -> a
totalCapacityWith f (Allarmtypes x) (GetCapacity c) =
  foldl (\a b -> let Capacity y = c b
                 in  a + f b y) 0 x

totalCapacity ::
  Allarmtypes a
  -> GetCapacity a
  -> Rational
totalCapacity =
  totalCapacityWith (const id)

totalArm ::
  Allarmtypes a
  -> GetCapacity a
  -> GetArm a
  -> Rational
totalArm t c r =
  totalCapacityWith (\b y -> y * ((^. armmeasure) . (r ^. _Wrapped) $ b)) t c

totalCapacityAndArm ::
  Allarmtypes a
  -> GetCapacity a
  -> GetArm a
  -> (Rational, Rational)
totalCapacityAndArm a c r =
  (totalCapacity a c, totalArm a c r)

totalCapacityAndArm2 ::
  Allarmtypes a
  -> GetCapacity a
  -> GetArm a
  -> Point 2 Rational
totalCapacityAndArm2 a c r =
  point2 (totalCapacity a c) (totalArm a c r)

capacityLimits ::
  Limits a
  -> GetCapacity a
  -> Limits a
capacityLimits (Limits x) (GetCapacity c) =
  Limits ((\(Limit s (Capacity d)) -> let z = foldl (\a b -> let Capacity y = c b in a + y) 0 s in Limit s (Capacity (d - z))) <$> x)

----

c172NormalCategory :: 
  SimplePolygon () Rational
c172NormalCategory =
  fromPoints . map ext $
    [
      point2 120.5 2550
    , point2 71 1500
    , point2 61 1500
    , point2 89 2200
    , point2 82.5 2200
    , point2 104.5 2550
    ]

c172UtilityCategory :: 
  SimplePolygon () Rational
c172UtilityCategory =
  fromPoints . map ext $
    [
      point2 61 1500    
    , point2 89 2200
    , point2 82.5 2200
    , point2 68 1950
    , point2 52.5 1500
    ]

testPoint ::
  Point 2 Rational
testPoint = 
  point2 70 2100
  
testPoint2 ::
  Point 2 Rational
testPoint2 = 
  point2 50 2050

nearestPoints ::
  CSeq (Rational, Point 2 Rational)
nearestPoints =
  sqDistanceToArg testPoint2 . supportingLine <$> outerBoundaryEdges c172UtilityCategory


{-

VH-LSE

Weight (lbs): 1663.2
Arm (in): 40.719
Moment (lb-in): 67724
Basic Empty Weight

    Weight (lbs): 1691.6
    Arm (in): 40.600
    Moment (lb-in): 68679

-- https://hackage.haskell.org/package/hgeometry-0.5.0.0/docs/Data-Geometry-Polygon.html

-}
----

samplePolygon :: 
  SimplePolygon () Rational
samplePolygon =
  fromPoints . map ext $
    [
      point2 2 1
    , point2 5 7
    , point2 8 10
    , point2 9 10
    , point2 7 4
    , point2 5 2
    ]    

testinpolygon ::
  [PointLocationResult]
testinpolygon =
  [
    point2 0 0 `inPolygon` samplePolygon
  , point2 1 1 `inPolygon` samplePolygon
  , point2 10 0 `inPolygon` samplePolygon
  , point2 5 13 `inPolygon` samplePolygon
  , point2 5 10 `inPolygon` samplePolygon
  , point2 10 5 `inPolygon` samplePolygon
  , point2 20 5 `inPolygon` samplePolygon
  ]

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
inner :: Int -> Maybe Int
inner x = 
  if x < 89
    then Just (25*x-25)
    else Nothing

---- 
---- Moment Envelope


polygon1 :: [(Double, Double)]
polygon1 = [(120.5, 2550), (71, 1500), (52.5,1500), (68,1950), (104.5, 2550)]

polygon2 :: [(Double, Double)]
polygon2 = [(61, 1500), (89, 2200), (82.5, 2200), (68,1950)]


myaxis :: Axis B V2 Double
myaxis = r2Axis &~ do
  linePlot' polygon1
  linePlot' polygon2

main :: IO ()
main = r2AxisMain myaxis

----

showRationalPoint ::
  (Arity d) => 
  Point d Rational
  -> String
showRationalPoint (Point (Vector v)) =
  concat
    [
      "Point"
    , show (FV.length v)
    , " "
    , show (map rational2Double (toList v))
    ]

rational2Double ::
  Rational
  -> Double
rational2Double = 
  fromRational
