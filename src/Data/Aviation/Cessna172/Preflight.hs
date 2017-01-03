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
import Control.Lens(makeClassy, lens, view, review, over, both, _head, Cons, Snoc, snoc, (^?), (&~), (.=), (*=), (%=), (%~), (.~), (^.), (&), _1)
import Control.Monad.State(State)
import Data.Foldable(toList)
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
import Diagrams.TwoD.Align(centerX)
import Diagrams.TwoD.Combinators(vcat')
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text(Text, alignedText, fontSizeL, font)
import Diagrams.Util(with)
import Plots(Axis, r2Axis, linePlot, plotColor, xLabel, yLabel, xMin, yMin, xMax, yMax, xAxis, yAxis, 
             axisLabelPosition, (&=), AxisLabelPosition(MiddleAxisLabel), axisLabelStyle, tickLabelStyle, scaleAspectRatio, 
             minorGridLines, visible, axisLabelGap, axisLabelTextFunction, minorTicksHelper, minorTicksFunction, majorTicksStyle, 
             majorGridLinesStyle, minorGridLinesStyle, lineStyle, majorTicksFunction, atMajorTicks, tickLabelFunction)
import Data.CircularSeq(CSeq)
import Data.Ext(ext, _core)
import Data.Geometry.Boundary(PointLocationResult)
import Data.Geometry.Line.Internal(sqDistanceToArg, supportingLine)
import Data.Geometry.Point(Point, point2, _point2)
import Data.Geometry.Polygon(SimplePolygon, Polygon, inPolygon, fromPoints, outerBoundaryEdges, outerBoundary)
import Data.Semigroup((<>))
import Diagrams.TwoD.Text(TextAlignment(BoxAlignedText))
import Plots.Axis.Render(renderAxis)

import Data.Aviation.Cessna172.Preflight.Arm(Arm, ArmStatic, HasArmStatic(armStatic), rangeArm, staticArm, (.->.))
import Data.Aviation.Cessna172.Preflight.Moment
import Data.Aviation.Cessna172.Preflight.Weight(Weight)
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
  C172Arms Arm
c172ArmsPOH =
  C172Arms
    (rangeArm (37 ^. inches) (34 ^. inches .->. 46 ^. inches))
    (staticArm (73 ^. inches))
    (staticArm (48 ^. inches))
    (rangeArm (95 ^. inches) (82 ^. inches .->. 108 ^. inches))
    (rangeArm (123 ^. inches) (108 ^. inches .->. 142 ^. inches))

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

c172ArmsAircraft ::
  ArmStatic
  -> C172AircraftArms Arm
c172ArmsAircraft a =
  C172AircraftArms
    (staticArm a)
    c172ArmsPOH

c172Moment :: 
  (HasArmStatic s, Applicative f) =>
  f Weight
  -> f s
  -> f Moment
c172Moment wt b =
  (\w -> Moment w . view armStatic) <$> wt <*> b

vhafrWeight ::
  C172Arms Weight
  -> C172AircraftArms Weight
vhafrWeight =
  C172AircraftArms
    (1684.3 ^. pounds)

vhafrArms ::
  C172AircraftArms Arm
vhafrArms =
  c172ArmsAircraft (39.37 ^. inches)

vhafrMoment ::
  C172Arms Weight
  -> C172AircraftArms Moment
vhafrMoment wt =
  c172Moment (vhafrWeight wt) vhafrArms

vhlseWeight ::
  C172Arms Weight
  -> C172AircraftArms Weight
vhlseWeight =
  C172AircraftArms
    (1691.6 ^. pounds)

vhlseArms ::
  C172AircraftArms Arm
vhlseArms =
  c172ArmsAircraft (40.6 ^. inches)

vhlseMoment ::
  C172Arms Weight
  -> C172AircraftArms Moment
vhlseMoment wt =
  c172Moment (vhlseWeight wt) vhlseArms

----

totalMomentPoundInchesPoint ::
  (HasMoment moment, Foldable f) =>
  f moment
  -> Point 2 Rational
totalMomentPoundInchesPoint x =
  let Moment w a = totalMoment pounds inches x
  in  point2 (review inches a) (review pounds w)

vhafrArmsAndWeight ::
  C172Arms Weight
  -> Point 2 Rational
vhafrArmsAndWeight =
  totalMomentPoundInchesPoint . vhafrMoment

vhlseArmsAndWeight ::
  C172Arms Weight
  -> Point 2 Rational
vhlseArmsAndWeight =
  totalMomentPoundInchesPoint . vhlseMoment

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
