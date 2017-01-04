{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aviation.Preflight where

import Prelude
import Control.Lens(view, review, over, both, _head, Cons, Snoc, snoc, (^?), (&~), (.=), (*=), (%=), (.~), (^.), (&))
import Control.Monad.State(State)
import Data.Foldable(toList)
import Data.Monoid(Any)
import Diagrams.Attributes(lwO, _lw)
import Diagrams.Prelude(V2(V2), black, red, lightgrey, darkgrey, local, _fontSize, rotateBy, mkSizeSpec, (#), fc)
import Diagrams.Backend.Cairo(Cairo(Cairo), OutputType(PNG, PDF, PS, SVG))
import Diagrams.Backend.Cairo.Internal(Options(CairoOptions)) -- (CairoOptions(..))
import Diagrams.Combinators(sep)
import Diagrams.Core.Compile(renderDia)
import Diagrams.Core.Style(HasStyle)
import Diagrams.Core.Types(QDiagram, Renderable)
import Diagrams.Path(Path)
import Diagrams.TwoD.Align(centerX)
import Diagrams.TwoD.Attributes(lc)
import Diagrams.TwoD.Combinators(vcat')
import Diagrams.TwoD.Text(Text, alignedText, fontSizeL, font)
import Diagrams.Util(with)
import Diagrams.TwoD.Text(TextAlignment(BoxAlignedText))
import Data.CircularSeq(CSeq)
import Data.Ext(ext, _core)
import Data.Geometry.Boundary(PointLocationResult(Inside, Outside, OnBoundary))
import Data.Geometry.Point(Point, point2, _point2)
import Data.Geometry.Polygon(SimplePolygon, Polygon, inPolygon, fromPoints, outerBoundary)
import Data.Semigroup((<>))
import Data.Aviation.C172(C172Arms(C172Arms), C172AircraftArms(C172AircraftArms), c172ArmsAircraft)
import Data.Aviation.Units(inches, pounds, kilograms, thouinches)
import Data.Aviation.WB(Arm, Moment(Moment), HasMoment, totalMoment, momentX, Weight)
import Plots(Axis, r2Axis, linePlot, plotColor, xLabel, yLabel, xMin, yMin, xMax, yMax, xAxis, yAxis, 
             axisLabelPosition, (&=), AxisLabelPosition(MiddleAxisLabel), axisLabelStyle, tickLabelStyle, scaleAspectRatio, 
             minorGridLines, visible, axisLabelGap, axisLabelTextFunction, minorTicksHelper, minorTicksFunction, majorTicksStyle, 
             majorGridLinesStyle, minorGridLinesStyle, lineStyle, majorTicksFunction, atMajorTicks, tickLabelFunction)
import Plots.Axis.Render(renderAxis)
import Text.Printf


----

totalMomentPoundInchesPoint ::
  (HasMoment moment, Foldable f) =>
  f moment
  -> Point 2 Rational
totalMomentPoundInchesPoint x =
  let Moment w a = totalMoment pounds inches x
  in  point2 (review thouinches a) (review pounds w)

----

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

c172sNormalCategory :: 
  SimplePolygon () Rational
c172sNormalCategory =
  fromPoints . map ext $
    [
      point2 120.5 2550
    , point2 71 1500
    , point2 61 1500
    , point2 89 2200
    , point2 82.5 2200
    , point2 104.5 2550
    ]

c172sUtilityCategory :: 
  SimplePolygon () Rational
c172sUtilityCategory =
  fromPoints . map ext $
    [
      point2 61 1500    
    , point2 89 2200
    , point2 82.5 2200
    , point2 68 1950
    , point2 52.5 1500
    ]

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

        linePlotPolygon c172sUtilityCategory black 0.7
        linePlotPolygon c172sNormalCategory black 0.7
        
        xLabel .= "Loaded Airplane Moment/1000 (Pounds - Inches)"
        yLabel .= "Loaded Airplane Weight (Pounds)"

        xMin .= Just 50
        yMin .= Just 1460

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
        minorGridLinesStyle %= lc lightgrey
        majorGridLinesStyle %= lwO 0.8
        majorGridLinesStyle %= lc darkgrey
        tickLabelFunction .= atMajorTicks (show . (round :: Double -> Int))
        minorGridLines . visible .= True

crosshairplot :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  Point 2 Rational
  -> Axis b V2 Double
crosshairplot pq =
  let (p, q) = _point2 pq
      crosshair = [[(p, q - 50), (p, q + 50)], [(p - 5, q), (p + 5, q)]]
      draw = mapM_ (\xx -> map (over both fromRational) xx `linePlot`
                             do  plotColor .= red
                                 lineStyle . _lw .= 1.5
                           ) crosshair
  in  plot draw

renderResult ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  Point 2 Rational
  -> QDiagram b V2 Double Any
renderResult p = 
  renderAxis (crosshairplot p) # centerX # dejavuSansMono
   
renderTextResult ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  Point 2 Rational
  -> QDiagram b V2 Double Any
renderTextResult pq =
  let (p, q) =
        _point2 pq
      textRational r =
        printf "%.2f" (fromRational r :: Double)
      utility =
        pq `inPolygon` c172sUtilityCategory
      normal =
        pq `inPolygon` c172sNormalCategory
      textPointLocationResult Inside =
        "YES"
      textPointLocationResult Outside = 
        "NO"
      textPointLocationResult OnBoundary = 
        "NO"
      reporttext a b x =
        alignedText a b x # fontSizeL 6 # dejavuSansMono # fc red
  in  vcat' (with & sep .~ 15)
        [
          renderResult pq
          , reporttext (0.650) (-1.65) ("Moment              " ++ textRational (p * 1000) ++ " pound/inches")
          , reporttext (0.805) (-2.20) ("All Up Weight       " ++ textRational q ++ " pounds")
          , reporttext (1.245) (-2.20) ("Utility Category    " ++ textPointLocationResult utility)
          , reporttext (1.190) (-2.80) ("Normal Category     " ++ textPointLocationResult normal)
        ]

----

sampleC172ArmWeights ::
  C172Arms Weight
sampleC172ArmWeights =
  C172Arms
    (80 ^. kilograms <> 55 ^. kilograms) -- Tony + Jess
    (85 ^. kilograms) -- George
    (336 ^. pounds) -- max fuel
    (10 ^. kilograms)
    mempty

vhafrMomentPoint ::
  C172Arms Weight
  -> Point 2 Rational
vhafrMomentPoint =
  totalMomentPoundInchesPoint . vhafrMoment

vhlseMomentPoint ::
  C172Arms Weight
  -> Point 2 Rational
vhlseMomentPoint =
  totalMomentPoundInchesPoint . vhlseMoment

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
  momentX (vhafrWeight wt) vhafrArms

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
  momentX (vhlseWeight wt) vhlseArms

main ::
  IO ()
main =
  let example = vhlseMomentPoint sampleC172ArmWeights
      pngoptions = CairoOptions
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
  in  mapM_ (\o -> fst (renderDia Cairo o (renderTextResult example))) [pngoptions, psoptions, pdfoptions, svgoptions]
