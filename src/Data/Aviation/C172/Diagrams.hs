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
, plotmomentpoint
, textreportDiagram
, plotMomentDiagram
, momentDiagram
, makepoly
) where

import Control.Applicative((<*>))
import Control.Category((.))
import Control.Lens(view, preview, over, both, _head, snoc, (&~), (.=), (*=), (%=), (.~), (&))
import Control.Monad.State(State)
import Data.Bool(Bool(True))
import Data.Colour(Colour)
import Data.Foldable(toList, mapM_)
import Data.Function(($))
import Data.Functor(fmap)
import Data.Maybe(Maybe(Just), maybe)
import Data.Monoid(Any)
import Diagrams.Attributes(lwO, _lw)
import Diagrams.Prelude(V2, black, red, green, lightgrey, darkgrey, local, _fontSize, rotateBy, (#), fc)
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
import Data.CircularSeq(CSeq)
import Data.Ext(ext, _core)
import Data.Geometry.Boundary(PointLocationResult(Inside, Outside, OnBoundary))
import Data.Geometry.Point(Point, point2, _point2)
import Data.Geometry.Polygon(SimplePolygon, Polygon, inPolygon, fromPoints, outerBoundary)
import Data.Semigroup((<>))
import Data.Tuple(uncurry)
import Plots(Axis, r2Axis, linePlot, plotColor, xLabel, yLabel, xMin, yMin, xMax, yMax, xAxis, yAxis, 
             axisLabelPosition, (&=), AxisLabelPosition(MiddleAxisLabel), axisLabelStyle, tickLabelStyle, scaleAspectRatio, 
             minorGridLines, visible, axisLabelGap, axisLabelTextFunction, minorTicksHelper, minorTicksFunction, majorTicksStyle, 
             majorGridLinesStyle, minorGridLinesStyle, lineStyle, majorTicksFunction, atMajorTicks, tickLabelFunction)
import Plots.Axis.Render(renderAxis)
import Prelude(Rational, Double, Int, Fractional((/)), fromRational, (*), (+), (-), show, round)
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

plotmomentpoint :: 
  Point 2 Rational
  -> [(Point 2 Rational, Point 2 Rational)]
plotmomentpoint pq =
  let (p, q) =
        _point2 pq
  in  [
        (point2 p (q - 50), point2 p (q + 50))
      , (point2 (p - 5) q, point2 (p + 5) q)
      ]

textreportDiagram :: 
  Renderable (Text Double) b =>
  SimplePolygon () Rational
  -> SimplePolygon () Rational
  -> Point 2 Rational
  -> (Point 2 Rational, Point 2 Rational)
  -> QDiagram b V2 Double Any
textreportDiagram u n pq (zfw, ffw) =
  let (p, q) =
        _point2 pq
      (zfwp, zfwq) =
        _point2 zfw
      (ffwp, ffwq) =
        _point2 ffw
      textRational r =
        printf "%.2f" (fromRational r :: Double)
      utility =
        pq `inPolygon` u
      normal =
        pq `inPolygon` n
      textPointLocationResult Inside =
        "YES"
      textPointLocationResult Outside = 
        "NO"
      textPointLocationResult OnBoundary = 
        "NO"
      reporttext a b x c =
        alignedText a b x # fontSizeL 3 # dejavuSansMono # fc c
  in  vcat' (with & sep .~ 15)
        [
            reporttext (0.970) (-04.80) ("Moment                           " <> textRational (p * 1000) <> " pound/inches") red
          , reporttext (1.136) (-06.80) ("All Up Weight                    " <> textRational q <> " pounds") red
          , reporttext (1.525) (-08.80) ("Utility Category                 " <> textPointLocationResult utility) red
          , reporttext (1.483) (-10.80) ("Normal Category                  " <> textPointLocationResult normal) red
          , reporttext (0.990) (-12.80) ("Zero Fuel Moment                 " <> textRational (zfwp * 1000) <> " pound/inches") green
          , reporttext (1.137) (-14.80) ("Zero Fuel Weight                 " <> textRational zfwq <> " pounds") green
          , reporttext (0.972) (-16.80) ("Fuel at Capacity Moment          " <> textRational (ffwp * 1000) <> " pound/inches") green
          , reporttext (1.138) (-18.80) ("Fuel at Capacity Weight          " <> textRational ffwq <> " pounds") green
        ]
 
plotMomentDiagram :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  SimplePolygon () Rational
  -> SimplePolygon () Rational
  -> [(Point 2 Rational, Point 2 Rational)]
  -> (Point 2 Rational, Point 2 Rational)
  -> QDiagram b V2 Double Any
plotMomentDiagram u n x fl =
  let r = r2Axis &~ 
            do  plotgrid
                plotenvelope [u, n]
                plotlines red 1.5 x
                plotlines green 1.5 [fl]
  in  renderAxis r # centerX # dejavuSansMono

momentDiagram ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  SimplePolygon () Rational
  -> SimplePolygon () Rational
  -> Point 2 Rational
  -> (Point 2 Rational, Point 2 Rational)
  -> QDiagram b V2 Double Any
momentDiagram u n pq fl =
  vcat' (with & sep .~ 15)
    [
      plotMomentDiagram u n (plotmomentpoint pq) fl
    , textreportDiagram u n pq fl
    ]

makepoly ::
  [(r, r)]
  -> SimplePolygon () r
makepoly = 
  fromPoints . fmap (ext . uncurry point2)
