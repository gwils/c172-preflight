{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aviation.C172.Diagrams(
  dejavuSansMono
, polygonPoint
, plotgrid
, plotlines
, plotenvelope
, crosshair
, textreportDiagram
, plotMomentDiagram
, titleDiagram
, momentDiagram
, zfwMomentPoint
, ffwMomentPoint
, ufwMomentPoint
, makepoly
, c172sNormalCategoryPoly
, c172sUtilityCategoryPoly
, totalMomentPoundInchesPoint
) where

import Control.Applicative((<*>))
import Control.Category((.))
import Control.Lens(view, preview, review, over, both, _head, snoc, (&~), (.=), (*=), (%=), (.~), (&), (%~))
import Control.Monad.State(State)
import Data.Aviation.C172.C172AircraftArms
import Data.Aviation.C172.C172MomentEnvelope
import Data.Aviation.Units
import Data.Aviation.WB.Moment
import Data.Bool(Bool(True))
import Data.Colour(Colour)
import Data.Foldable(Foldable, toList, mapM_)
import Data.Function(($))
import Data.Functor(fmap)
import Data.CircularSeq(CSeq)
import Data.Ext(ext, _core)
import Data.Geometry.Boundary(PointLocationResult(Inside, Outside, OnBoundary))
import Data.Geometry.Point(Point, point2, _point2, xCoord, yCoord)
import Data.Geometry.Polygon(SimplePolygon, Polygon, inPolygon, fromPoints, outerBoundary)
import Data.Maybe(Maybe(Just), maybe)
import Data.Monoid(Any)
import Diagrams.Attributes(lwO, _lw)
import Diagrams.Prelude(V2, black, red, green, orange, lightgrey, darkgrey, local, _fontSize, rotateBy, (#), fc)
import Diagrams.Combinators(sep)
import Diagrams.Core.Measure(Measure)
import Diagrams.Core.Style(HasStyle)
import Diagrams.Core.Types(QDiagram, Renderable)
import Diagrams.Path(Path)
import Diagrams.TwoD.Align(centerX)
import Diagrams.TwoD.Attributes(lc)
import Diagrams.TwoD.Combinators(vcat')
import Diagrams.TwoD.Text(Text, alignedText, fontSizeL, font)
import Diagrams.Util(with)
import Diagrams.TwoD.Text(TextAlignment(BoxAlignedText))
import Data.Semigroup((<>))
import Data.String(String)
import Data.Tuple(uncurry)
import Plots(Axis, r2Axis, linePlot, plotColor, xLabel, yLabel, xMin, yMin, xMax, yMax, xAxis, yAxis, 
             axisLabelPosition, (&=), AxisLabelPosition(MiddleAxisLabel), axisLabelStyle, tickLabelStyle, scaleAspectRatio, 
             minorGridLines, visible, axisLabelGap, axisLabelTextFunction, minorTicksHelper, minorTicksFunction, majorTicksStyle, 
             majorGridLinesStyle, minorGridLinesStyle, lineStyle, majorTicksFunction, atMajorTicks, tickLabelFunction)
import Plots.Axis.Render(renderAxis)
import Prelude(Rational, Double, Int, Fractional((/)), fromRational, (*), (+), show, round, subtract)
import Text.Printf(printf)

dejavuSansMono ::
  HasStyle a =>
  a
  -> a
dejavuSansMono =
  font "DejaVu Sans Mono"

polygonPoint ::
  Fractional b =>
  Polygon t extra Rational ->
  CSeq (b, b)
polygonPoint =
  fmap (over both fromRational . _point2 . _core) . view outerBoundary

plotgrid ::
  State (Axis b V2 Double) ()
plotgrid =
  do  xLabel .= "Loaded Airplane Moment/1000 (Pounds - Inches)"
      yLabel .= "Loaded Airplane Weight (Pounds)"

      xMin .= Just 50
      yMin .= Just 1460

      xMax .= Just 130
      yMax .= Just 2600

      xAxis &= do
        scaleAspectRatio .= Just 11
        majorTicksFunction .= \_ -> [50, 60..130]

      yAxis &= do
        axisLabelTextFunction %= \f _ s -> f (BoxAlignedText 0.5 0.5) s # rotateBy (1/4)
        axisLabelGap *= 2
        majorTicksFunction .= \_ -> [1500, 1600..2600]

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

plotlines :: 
  Renderable (Path V2 Double) b =>
  Colour Double
  -> Measure Double
  -> [(Point 2 Rational, Point 2 Rational)]
  -> State (Axis b V2 Double) ()
plotlines c w =
  mapM_ (\(a, b) ->
    fmap (over both fromRational) [_point2 a, _point2 b] `linePlot`
      do  plotColor .= c
          lineStyle . _lw .= w)
         
plotenvelope :: 
  (Renderable (Path V2 Double) b) =>
  [SimplePolygon () Rational]
  -> State (Axis b V2 Double) ()
plotenvelope =
  let snochead =
        maybe <*> snoc <*> preview _head
      linePlotPolygon x c l = (linePlot . snochead  . toList . polygonPoint  $ x) $ 
        do  plotColor .= c
            lineStyle . _lw .= l
  in  mapM_ (\g -> linePlotPolygon g black 0.7)

crosshair ::
  Rational
  -> Rational
  -> Point 2 Rational
  -> [(Point 2 Rational, Point 2 Rational)]
crosshair a b pq =
  [
    (pq & yCoord %~ (subtract a), pq & yCoord %~ (+ a))
  , (pq & xCoord %~ (subtract b), pq & xCoord %~ (+ b))
  ]

textreportDiagram :: 
  Renderable (Text Double) b =>
  C172AircraftArms Moment
  -> QDiagram b V2 Double Any
textreportDiagram m =
  let pq =
        totalMomentPoundInchesPoint m
      (p, q) =
        _point2 pq
      (zfwp, zfwq) =
        _point2 (zfwMomentPoint m)
      (ufwp, ufwq) =
        _point2 (ufwMomentPoint m)
      (ffwp, ffwq) =
        _point2 (ffwMomentPoint m)
      textRational r =
        printf "%.2f" (fromRational r :: Double)
      utility =
        pq `inPolygon` c172sUtilityCategoryPoly
      normal =
        pq `inPolygon` c172sNormalCategoryPoly
      textPointLocationResult Inside =
        "YES"
      textPointLocationResult Outside = 
        "NO"
      textPointLocationResult OnBoundary = 
        "NO"
      reporttext a b x c =
        alignedText a b x # fontSizeL 5 # dejavuSansMono # fc c
  in  vcat' (with & sep .~ 15)
        [
            reporttext (0.650) (-02.80) ("All Up Moment                    " <> textRational (p * 1000) <> " lb/in") red
          , reporttext (0.725) (-03.80) ("All Up Weight                    " <> textRational q <> " lb") red
          , reporttext (0.890) (-04.80) ("Utility Category                 " <> textPointLocationResult utility) red
          , reporttext (0.865) (-05.80) ("Normal Category                  " <> textPointLocationResult normal) red
          , reporttext (0.663) (-06.80) ("Zero Fuel Moment                 " <> textRational (zfwp * 1000) <> " lb/in") green
          , reporttext (0.725) (-07.80) ("Zero Fuel Weight                 " <> textRational zfwq <> " lb") green
          , reporttext (0.663) (-08.80) ("Usable Fuel Moment               " <> textRational (ufwp * 1000) <> " lb/in") orange
          , reporttext (0.724) (-09.80) ("Usable Fuel Weight               " <> textRational ufwq <> " lb") orange
          , reporttext (0.650) (-10.80) ("Fuel at Capacity Moment          " <> textRational (ffwp * 1000) <> " lb/in") green
          , reporttext (0.726) (-11.80) ("Fuel at Capacity Weight          " <> textRational ffwq <> " lb") green 
        ]
 
plotMomentDiagram :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  C172AircraftArms Moment
  -> QDiagram b V2 Double Any
plotMomentDiagram m =
  let auwMomentPlot = crosshair 40 4 (totalMomentPoundInchesPoint m)
      ufwMomentPlot = crosshair 20 2 (ufwMomentPoint m)
      fl = (zfwMomentPoint m, ffwMomentPoint m)
      r = r2Axis &~ 
            do  plotgrid
                plotenvelope [c172sUtilityCategoryPoly, c172sNormalCategoryPoly]
                plotlines red 1.5 auwMomentPlot
                plotlines orange 1.0 ufwMomentPlot
                plotlines green 1.5 [fl]
  in  renderAxis r # centerX # dejavuSansMono

titleDiagram ::
  Renderable (Text Double) b =>
  String
  -> QDiagram b V2 Double Any
titleDiagram s =
  alignedText 0.5 (-10) s # fontSizeL 5 # dejavuSansMono # fc black

momentDiagram ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  String
  -> C172AircraftArms Moment
  -> QDiagram b V2 Double Any
momentDiagram s m =
  vcat' (with & sep .~ 10)
    [
      plotMomentDiagram m
    , textreportDiagram m
    , titleDiagram s
    ]

zfwMomentPoint ::
  C172AircraftArms Moment
  -> Point 2 Rational
zfwMomentPoint =
  totalMomentPoundInchesPoint . zfwMoment

ffwMomentPoint ::
  C172AircraftArms Moment
  -> Point 2 Rational
ffwMomentPoint =
  totalMomentPoundInchesPoint . ffwMoment

ufwMomentPoint ::
  C172AircraftArms Moment
  -> Point 2 Rational
ufwMomentPoint =
  totalMomentPoundInchesPoint . ufwMoment

makepoly ::
  [(r, r)]
  -> SimplePolygon () r
makepoly = 
  fromPoints . fmap (ext . uncurry point2)

c172sNormalCategoryPoly :: 
  SimplePolygon () Rational
c172sNormalCategoryPoly =
  makepoly c172sNormalCategory

c172sUtilityCategoryPoly :: 
  SimplePolygon () Rational
c172sUtilityCategoryPoly =
  makepoly c172UtilityCategory

totalMomentPoundInchesPoint ::
  (HasMoment moment, Foldable f) =>
  f moment
  -> Point 2 Rational
totalMomentPoundInchesPoint x =
  let mm = totalMoments pounds inches x
      ww = totalWeights x
  in  point2 (mm / 1000) (review pounds ww)
