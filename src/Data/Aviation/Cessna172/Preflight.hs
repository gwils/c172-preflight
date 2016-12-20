{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aviation.Cessna172.Preflight where

import Prelude
import Control.Applicative(liftA2)
import Data.Foldable(toList, fold)
import Diagrams.Prelude(V2)
import Diagrams.Backend.Rasterific.CmdLine(B)
import Plots(Axis, r2Axis, r2AxisMain, linePlot')
import Control.Lens(Prism', Lens', makeClassy, makeWrapped, _Wrapped, prism', lens, view, set, over, both, _head, Cons, Snoc, snoc, (^?), (&~))
import Data.CircularSeq(CSeq)
import Data.Ext(ext, _core)
import Data.Geometry.Boundary(PointLocationResult)
import Data.Geometry.Line.Internal(sqDistanceToArg, supportingLine)
import Data.Geometry.Point(Point(Point), point2, _point2)
import Data.Geometry.Polygon(SimplePolygon, Polygon, inPolygon, fromPoints, outerBoundaryEdges, outerBoundary)
import Data.Geometry.Vector(Arity, Vector(Vector))
import qualified Data.Vector.Fixed as FV(length)

data MeasuredArm =
  MeasuredArm {
    _armmeasure ::
      Rational -- inches
  , _armrange ::
      Maybe (Rational, Rational)
  } deriving (Eq, Ord, Show)

makeClassy ''MeasuredArm

measuredArmNorange :: 
  Rational
  -> MeasuredArm
measuredArmNorange n =
  MeasuredArm n Nothing

data C172Arms a =
  C172Arms {
    _frontseat ::
      a
  , _rearseat ::
      a
  , _fuel ::
      a
  , _baggagea ::
      a
  , _baggageb ::
      a
  }
  deriving (Eq, Ord, Show)

makeClassy ''C172Arms

instance Functor C172Arms where
  fmap k (C172Arms t r f a b) =
    C172Arms (k t) (k r) (k f) (k a) (k b)

instance Applicative C172Arms where
  pure a =
    C172Arms a a a a a
  C172Arms f1 f2 f3 f4 f5 <*> C172Arms a1 a2 a3 a4 a5 =
    C172Arms (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5)

instance Foldable C172Arms where
  foldr k z (C172Arms t r f a b) =
    foldr k z [t,r,f,a,b]

instance Traversable C172Arms where
  traverse k (C172Arms t r f a b) =
    C172Arms <$> k t <*> k r <*> k f <*> k a <*> k b

data Weight =
  Weight
    Rational -- todo units
  deriving (Eq, Ord, Show)

makeWrapped ''Weight
makeClassy ''Weight

instance Monoid Weight where
  mempty =
    Weight 0
  Weight a `mappend` Weight b =
    Weight (a + b)
    
c172ArmsPOH ::
  C172Arms MeasuredArm
c172ArmsPOH =
  C172Arms
    (MeasuredArm 37 (Just (34, 46)))
    (measuredArmNorange 48)
    (measuredArmNorange 73)
    (MeasuredArm 95 (Just (82, 108)))
    (MeasuredArm 123 (Just (108, 142)))

applying ::
  Applicative f =>
  Lens' a b
  -> Lens' (f a) (f b)
applying k =
  lens (fmap (view k)) (\c1 c2 -> flip (set k) <$> c1 <*> c2)

applyingMeasuredArm ::
  (Applicative f, HasMeasuredArm a) =>
  Lens' (f a) (f MeasuredArm)
applyingMeasuredArm = 
  applying measuredArm

applyingWeight ::
  (Applicative f, HasWeight a) =>
  Lens' (f a) (f Weight)
applyingWeight = 
  applying weight

newtype Moment =
  Moment
    Rational
  deriving (Eq, Ord, Show)

makeWrapped ''Moment
makeClassy ''Moment

instance Monoid Moment where
  mempty =
    Moment 0
  Moment a `mappend` Moment b =
    Moment (a + b)

calculateMoment ::
  (HasMeasuredArm arm, HasWeight weight) =>
  arm
  -> weight
  -> Moment
calculateMoment a w =
  Moment (view armmeasure a * view (weight . _Wrapped) w)

calculateMoments ::
  (Applicative f, HasMeasuredArm arm, HasWeight weight) =>
  f arm
  -> f weight
  -> f Moment
calculateMoments =
  liftA2 calculateMoment

----

data C172AircraftArms a =
  C172AircraftArms {
    _aircraftArm ::
      a
  , c172Arms_ ::
      C172Arms a
  }
  deriving (Eq, Ord, Show)

makeClassy ''C172AircraftArms

instance HasC172Arms (C172AircraftArms a) a where
  c172Arms =
    lens
      (\(C172AircraftArms _ c) -> c)
      (\(C172AircraftArms c _) a -> C172AircraftArms c a)

instance Functor C172AircraftArms where
  fmap k (C172AircraftArms c x) =
    C172AircraftArms (k c) (fmap k x)

instance Applicative C172AircraftArms where
  pure a =
    C172AircraftArms a (pure a)
  C172AircraftArms c1 c2 <*> C172AircraftArms x1 x2 =
    C172AircraftArms (c1 x1) (c2 <*> x2)

instance Foldable C172AircraftArms where
  foldr k z (C172AircraftArms c x) =
    k c (foldr k z x)

instance Traversable C172AircraftArms where
  traverse k (C172AircraftArms c x) =
    C172AircraftArms <$> k c <*> traverse k x

----

c172MeasuredArms ::
  Rational
  -> C172AircraftArms MeasuredArm
c172MeasuredArms a =
  C172AircraftArms
    (measuredArmNorange a)
    c172ArmsPOH

vhafrMeasuredArms ::
  C172AircraftArms MeasuredArm
vhafrMeasuredArms =
  c172MeasuredArms 39.37

vhafrWeight ::
  C172Arms Weight
  -> C172AircraftArms Weight
vhafrWeight =
  C172AircraftArms
    (Weight 1684.3)
    
vhlseMeasuredArms ::
  C172AircraftArms MeasuredArm
vhlseMeasuredArms =
  c172MeasuredArms 40.6

vhlseWeight ::
  C172Arms Weight
  -> C172AircraftArms Weight
vhlseWeight =
  C172AircraftArms
    (Weight 1691.6)

----

sumArmsAndWeight ::
  (HasWeight weight, HasMeasuredArm arm, Foldable f, Applicative f) =>
  f arm
  -> f weight
  -> Point 2 Rational
sumArmsAndWeight a w =
  let (Moment m, Weight x) = (fold (calculateMoments a w), foldMap (view weight) w)
  in  point2 (m) x

vhafrArmsAndWeight ::
  C172Arms Weight
  -> Point 2 Rational
vhafrArmsAndWeight =
  sumArmsAndWeight vhafrMeasuredArms . vhafrWeight

vhlseArmsAndWeight ::
  C172Arms Weight
  -> Point 2 Rational
vhlseArmsAndWeight =
  sumArmsAndWeight vhlseMeasuredArms . vhlseWeight

{-


vhafrMeasuredArms ::
  C172AircraftArms MeasuredArm
vhafrMeasuredArms =
  c172MeasuredArms 39.37

vhafrWeight ::
  C172Arms Weight
  -> C172AircraftArms Weight
vhafrWeight =

-}
----

sampleC172ArmWeights ::
  C172Arms Weight
sampleC172ArmWeights =
  Weight <$>
    C172Arms
      363.763
      176.37
      180
      22.0462
      2.20462

-- baggage "A" maximum 120lb
-- baggage "B" maximum 50lb
-- maximum overall baggage 120lb
{-

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
-}

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

nearestPoints ::
  SimplePolygon () Rational
  -> Point 2 Rational
  -> CSeq (Rational, Point 2 Rational)
nearestPoints y p =
  sqDistanceToArg p . supportingLine <$> outerBoundaryEdges y

{-

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

data C172ShortFieldChart =
  C172ShortFieldChart {
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

makeClassy ''C172ShortFieldChart

data TakeOffDistance =
  TakeOffDistance {
    _pa0000 ::
      C172ShortFieldChart
  , _pa1000 ::
      C172ShortFieldChart
  , _pa2000 ::
      C172ShortFieldChart
  , _pa3000 ::
      C172ShortFieldChart
  , _pa4000 ::
      C172ShortFieldChart
  , _pa5000 ::
      C172ShortFieldChart
  , _pa6000 ::
      C172ShortFieldChart
  , _pa7000 ::
      C172ShortFieldChart
  , _pa8000 ::
      C172ShortFieldChart
  }
  deriving (Eq, Ord, Show)

makeClassy ''TakeOffDistance

c172s_2550lbs_groundroll_takeoff ::
  TakeOffDistance
c172s_2550lbs_groundroll_takeoff =
  TakeOffDistance
    (
      C172ShortFieldChart
        860
        925
        995
        1070
        1150
    )
    (
      C172ShortFieldChart
        940
        1010
        1090
        1170
        1260
    )
    (
      C172ShortFieldChart
        1025
        1110
        1195
        1285
        1380
    )
    (
      C172ShortFieldChart
        1125
        1215
        1310
        1410
        1515
    )
    (
      C172ShortFieldChart
        1235
        1335
        1440
        1550
        1660
    )
    (
      C172ShortFieldChart
        1355
        1465
        1585
        1705
        1825
    )
    (
      C172ShortFieldChart
        1495
        1615
        1745
        1875
        2010
    )
    (
      C172ShortFieldChart
        1645
        1785
        1920
        2065
        2215
    )
    (
      C172ShortFieldChart
        1820
        1970
        2120
        2280
        2450
    )

c172s_2550lbs_50ft_takeoff ::
  TakeOffDistance
c172s_2550lbs_50ft_takeoff =
  TakeOffDistance
    (
      C172ShortFieldChart
        1465
        1575
        1690
        1810
        1945
    )
    (
      C172ShortFieldChart
        1600
        1720
        1850
        1990
        2135
    )
    (
      C172ShortFieldChart
        1755
        1890
        2035
        2190
        2355
    )
    (
      C172ShortFieldChart
        1925
        2080
        2240
        2420
        2605
    )
    (
      C172ShortFieldChart
        2120
        2295
        2480
        2685
        2880
    )
    (
      C172ShortFieldChart
        2345
        2545
        2755
        2975
        3205
    )
    (
      C172ShortFieldChart
        2605
        2830
        3075
        3320
        3585
    )
    (
      C172ShortFieldChart
        2910
        3170
        3440
        3730
        4045
    )
    (
      C172ShortFieldChart
        3265
        3575
        3880
        4225
        4615
    )

c172s_2550lbs_groundroll_landing ::
  TakeOffDistance
c172s_2550lbs_groundroll_landing =
  TakeOffDistance
    (
      C172ShortFieldChart
        545
        565
        585
        605
        625
    )
    (
      C172ShortFieldChart
        565
        585
        605
        625
        650
    )
    (
      C172ShortFieldChart
        585
        610
        630
        650
        670
    )
    (
      C172ShortFieldChart
        610
        630
        655
        675
        695
    )
    (
      C172ShortFieldChart
        630
        655
        675
        700
        725
    )
    (
      C172ShortFieldChart
        655
        680
        705
        725
        750
    )
    (
      C172ShortFieldChart
        680
        705
        730
        755
        780
    )
    (
      C172ShortFieldChart
        705
        730
        760
        785
        810
    )
    (
      C172ShortFieldChart
        735
        760
        790
        815
        840
    )

c172s_2550lbs_50ft_landing ::
  TakeOffDistance
c172s_2550lbs_50ft_landing =
  TakeOffDistance
    (
      C172ShortFieldChart
        1290
        1320
        1350
        1380
        1415
    )
    (
      C172ShortFieldChart
        1320
        1350
        1385
        1420
        1450
    )
    (
      C172ShortFieldChart
        1355
        1385
        1420
        1455
        1490
    )
    (
      C172ShortFieldChart
        1385
        1425
        1460
        1495
        1530
    )
    (
      C172ShortFieldChart
        1425
        1460
        1495
        1535
        1570
    )
    (
      C172ShortFieldChart
        1460
        1500
        1535
        1575
        1615
    )
    (
      C172ShortFieldChart
        1500
        1540
        1580
        1620
        1660
    )
    (
      C172ShortFieldChart
        1545
        1585
        1625
        1665
        1705
    )
    (
      C172ShortFieldChart
        1585
        1630
        1670
        1715
        1755
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
  -> (C172ShortFieldChart, C172ShortFieldChart, Int)
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
  -> C172ShortFieldChart
  -> (Int, Int, Int)
intervalsTemperature (Temperature n) (C172ShortFieldChart _0 _1 _2 _3 _4) =
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

{-
g :: Axis B V2 Double
g = 
  linePlot' (fmap (over both fromRational . _point2 . _core) (view outerBoundary c172UtilityCategory))
-}

snochead ::
  forall s a.
  (Cons s s a a, Snoc s s a a) =>
  s
  -> s
snochead x = 
  let h = x ^? _head
  in  case h of
        Nothing ->
          x
        Just i ->
          snoc x i

polygonPoint2 ::
  Fractional b =>
  Polygon t extra Rational ->
  CSeq (b, b)
polygonPoint2 =
  fmap (over both fromRational . _point2 . _core) . view outerBoundary

myaxis :: Axis B V2 Double
myaxis =
  let linePlotPolygon = linePlot' . snochead  . toList . polygonPoint2
  in  r2Axis &~ do
        linePlotPolygon c172NormalCategory
        linePlotPolygon c172UtilityCategory
        
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
