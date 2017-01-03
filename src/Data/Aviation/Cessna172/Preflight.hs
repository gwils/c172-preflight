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
import Control.Lens(Prism', Lens', makeClassy, makeWrapped, _Wrapped, prism', lens, view, review, set, over, both, _head, Cons, Snoc, snoc, (^?), (&~), (.=), (*=), (%=), (%~), (.~), (^.), (&), _1)
import Control.Monad.State(State)
import Data.Foldable(toList, fold)
import Data.Monoid(Any)
import Diagrams.Attributes(lwO, _lw)
import Diagrams.Prelude(V2(V2), black, red, local, _fontSize, rotateBy, mkSizeSpec, (#))
import Diagrams.Backend.Cairo(Cairo(Cairo), OutputType(PNG, PDF, PS, SVG))
import Diagrams.Backend.Cairo.Internal(Options(CairoOptions)) -- (CairoOptions(..))
import Diagrams.Combinators(sep)
import Diagrams.Core.Compile(renderDia)
import Diagrams.Core.Style(HasStyle)
import Diagrams.Core.Types(QDiagram, Renderable)
import Diagrams.Path(Path)
import Diagrams.TwoD.Align(centerX, alignL)
import Diagrams.TwoD.Combinators((===), vcat')
import Diagrams.TwoD.Ellipse(circle)
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text(Text, text, alignedText, fontSizeL, font)
import Diagrams.Util(with)
import Plots(Axis, r2Axis, linePlot, plotColor, xLabel, yLabel, xMin, yMin, xMax, yMax, xAxis, yAxis, 
             axisLabelPosition, (&=), AxisLabelPosition(MiddleAxisLabel), axisLabelStyle, tickLabelStyle, scaleAspectRatio, 
             minorGridLines, visible, axisLabelGap, axisLabelTextFunction, minorTicksHelper, minorTicksFunction, majorTicksStyle, 
             majorGridLinesStyle, minorGridLinesStyle, lineStyle, majorTicksFunction, atMajorTicks, tickLabelFunction)
import Data.CircularSeq(CSeq)
import Data.Ext(ext, _core)
import Data.Geometry.Boundary(PointLocationResult)
import Data.Geometry.Line.Internal(sqDistanceToArg, supportingLine)
import Data.Geometry.Point(Point(Point), point2, _point2)
import Data.Geometry.Polygon(SimplePolygon, Polygon, inPolygon, fromPoints, outerBoundaryEdges, outerBoundary)
import Data.Semigroup((<>))
import Data.Geometry.Vector(Arity, Vector(Vector))
import qualified Data.Vector.Fixed as FV(length)
import Diagrams.TwoD.Text(TextAlignment(BoxAlignedText))
import Plots.Axis.Render(renderAxis)

import Data.Aviation.Cessna172.Preflight.MeasuredArm(MeasuredArm, MeasuredArmStatic, HasMeasuredArmStatic(measuredArmStatic), rangeMeasuredArm, staticMeasuredArm, (.->.))
import Data.Aviation.Cessna172.Preflight.Weight(Weight, HasWeight(weight))
import Data.Aviation.Units(inches, pounds, kilograms)

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

c172ArmsPOH ::
  C172Arms MeasuredArm
c172ArmsPOH =
  C172Arms
    (rangeMeasuredArm (37 ^. inches) (34 ^. inches .->. 46 ^. inches))
    (staticMeasuredArm (48 ^. inches))
    (staticMeasuredArm (73 ^. inches))
    (rangeMeasuredArm (95 ^. inches) (82 ^. inches .->. 108 ^. inches))
    (rangeMeasuredArm (123 ^. inches) (108 ^. inches .->. 142 ^. inches))
  
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
  (HasMeasuredArmStatic arm, HasWeight weight) =>
  arm
  -> weight
  -> Moment
calculateMoment a w =
  let a' = review inches (view measuredArmStatic a)
      w' = review pounds (view weight w)
  in  Moment (a' * w')
  
calculateMoments ::
  (Applicative f, HasMeasuredArmStatic arm, HasWeight weight) =>
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
  MeasuredArmStatic
  -> C172AircraftArms MeasuredArm
c172MeasuredArms a =
  C172AircraftArms
    (staticMeasuredArm a)
    c172ArmsPOH

vhafrMeasuredArms ::
  C172AircraftArms MeasuredArm
vhafrMeasuredArms =
  c172MeasuredArms (39.37 ^. inches)

vhafrWeight ::
  C172Arms Weight
  -> C172AircraftArms Weight
vhafrWeight =
  C172AircraftArms
    (1684.3 ^. pounds)
    
vhlseMeasuredArms ::
  C172AircraftArms MeasuredArm
vhlseMeasuredArms =
  c172MeasuredArms (40.6 ^. inches)

vhlseWeight ::
  C172Arms Weight
  -> C172AircraftArms Weight
vhlseWeight =
  C172AircraftArms
    (1691.6 ^. pounds)

----

sumArmsAndWeight ::
  (HasMeasuredArmStatic arm, HasWeight weight, Foldable f, Applicative f) =>
  f arm
  -> f weight
  -> Point 2 Rational
sumArmsAndWeight a w =
  let (Moment m, x) = (fold (calculateMoments a w), foldMap (view weight) w)
  in  point2 m (review pounds x)
  
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

----

sampleC172ArmWeights ::
  C172Arms Weight
sampleC172ArmWeights =
  C172Arms
    (363.763 ^. pounds)
    (176.37 ^. pounds)
    (180 ^. pounds)
    (22.0462 ^. pounds)
    (2.20462 ^. pounds)

sampleC172ArmWeights2 ::
  C172Arms Weight
sampleC172ArmWeights2 =
  C172Arms
    (80 ^. kilograms <> 85 ^. kilograms) -- Tony + George
    (55 ^. kilograms) -- Jess
    (336 ^. pounds) -- max fuel
    (10 ^. kilograms)
    mempty

sampleC172ArmWeights3 ::
  C172Arms Weight
sampleC172ArmWeights3 =
  C172Arms
    (80 ^. kilograms <> 55 ^. kilograms) -- Tony + Jess
    (85 ^. kilograms) -- George
    (336 ^. pounds) -- max fuel
    (10 ^. kilograms)
    mempty

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

dejavuSansMono ::
  HasStyle a =>
  a
  -> a
dejavuSansMono =
  font "DejaVu Sans Mono"

plot :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  State (Axis b V2 Double) a
  -> Axis b V2 Double
plot z =
  let linePlotPolygon x c l = (linePlot . snochead  . toList . polygonPoint2  $ x) $ 
        do  plotColor .= c
            lineStyle . _lw .= l
  in  r2Axis &~ do
        _ <- z

        linePlotPolygon c172UtilityCategory black 0.7
        linePlotPolygon c172NormalCategory black 0.7
        
        xLabel .= "Loaded Airplane Moment/1000 (Pounds - Inches)"
        yLabel .= "Loaded Airplane Weight (Pounds)"

        xMin .= Just 50
        yMin .= Just 1480

        xMax .= Just 130
        yMax .= Just 2600

        xAxis &= do
          scaleAspectRatio .= Just 11
          majorTicksFunction .= \_ -> [50, 60, 70, 80, 90, 100, 110, 120, 130]

        yAxis &= do
          axisLabelTextFunction %= \f _ s -> f (BoxAlignedText 0.5 0.5) s # rotateBy (1/4)
          axisLabelGap *= 2
          majorTicksFunction .= \_ -> [1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600]

        axisLabelStyle . _fontSize .= local 8.5
        tickLabelStyle . _fontSize .= local 8.5
        axisLabelPosition .= MiddleAxisLabel
        minorTicksFunction .= minorTicksHelper 10
        majorTicksStyle %= lwO 1.6
        minorGridLinesStyle %= lwO 0.3
        majorGridLinesStyle %= lwO 0.6
        tickLabelFunction .= atMajorTicks (show . (round :: Double -> Int))
        minorGridLines . visible .= True

crosshairplot :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  Point 2 Rational
  -> Axis b V2 Double
crosshairplot pq =
  let (p, q) = _point2 pq & _1 %~ (/ 1000)
      crosshair = [[(p, q - 50), (p, q + 50)], [(p - 5, q), (p + 5, q)]]
      draw = mapM_ (\xx -> map (over both fromRational) xx `linePlot`
                             do  plotColor .= red
                                 lineStyle . _lw .= 1.5
                           ) crosshair
  in  plot draw

result ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  Axis b V2 Double
result =
  crosshairplot (vhlseArmsAndWeight sampleC172ArmWeights3)

renderResult ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  QDiagram b V2 Double Any
renderResult = 
  vcat' (with & sep .~ 5)
    [
      renderAxis result # centerX # dejavuSansMono
    , hrule 10
    , alignedText (1.8) (0.25) "this is some text\nthis is some more\nand more..." # fontSizeL 6 # dejavuSansMono]

main ::
  IO ()
main =
  let pngoptions = CairoOptions
                  "dist/output.png"
                  (mkSizeSpec (V2 (Just 800) (Just 1131.2)))
                  PNG
                  False
      psoptions = CairoOptions
                  "dist/output.ps"
                  (mkSizeSpec (V2 (Just 800) (Just 1131.2)))
                  PS
                  False
      pdfoptions = CairoOptions
                  "dist/output.pdf"
                  (mkSizeSpec (V2 (Just 800) (Just 1131.2)))
                  PDF
                  False
      svgoptions = CairoOptions
                  "dist/output.svg"
                  (mkSizeSpec (V2 Nothing Nothing))
                  SVG
                  False                  
  in  mapM_ (\o -> fst (renderDia Cairo o renderResult)) [pngoptions, psoptions, pdfoptions, svgoptions]

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

----

{-

module Avgas100LL(
  Kilograms(kilograms)
, Avgas100LL
) where

import Control.Lens(Iso', iso)
import Data.Ratio((%))
import Numeric.Lens

newtype Avgas100LL =
  Avgas100LL
    Rational -- normalise to pounds, hide constructor
  deriving (Eq, Ord, Show)

class Pounds a where
  pounds ::
    Iso' Rational a

instance Pounds Avgas100LL where
  pounds =
    iso
      Avgas100LL
      (\(Avgas100LL x) -> x)

class Kilograms a where
  kilograms ::    
    Iso' Rational a

instance Kilograms Avgas100LL where
  kilograms =
    let rate = 22046226218 % 10000000000
    in  multiplying rate . pounds

class USGallons a where
  usgallons ::
    Iso' Rational a

instance USGallons Avgas100LL where
  usgallons =
    let rate = 6 -- Cessna 172 PoH, Section 6 WEIGHT AND BALANCE, LOADING GRAPH 6-12
    in  multiplying rate . pounds

class ImperialGallons a where
  imperialgallons ::
    Iso' Rational a

instance ImperialGallons Avgas100LL where
  imperialgallons =
    let rate = 454609 % 100000
    in  multiplying rate . litres

class Litres a where
  litres ::
    Iso' Rational a

instance Litres Avgas100LL where
  litres =
    let rate = 1 / ((254 * 254 * 254 * 231) % 1000000000)
    in  multiplying rate . usgallons

-}