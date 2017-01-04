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
, momentDiagramFuelline
, makepoly
) where

import Control.Applicative((<*>))
import Control.Category((.))
import Control.Lens(view, preview, over, both, _head, snoc, (&~), (.=), (*=), (%=), (.~), (&))
import Control.Monad.State(State)
import Data.Bool(Bool(True))
import Data.Foldable(toList, mapM_)
import Data.Function(($))
import Data.Functor(fmap)
import Data.Maybe(Maybe(Just), maybe)
import Data.Monoid(Any)
import Diagrams.Attributes(lwO, _lw)
import Diagrams.Prelude(V2, black, red, lightgrey, darkgrey, local, _fontSize, rotateBy, (#), fc)
import Diagrams.Combinators(sep)
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

---- 
---- Moment Envelope

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
  [(Point 2 Rational, Point 2 Rational)]
  -> State (Axis b V2 Double) ()
plotlines =
  mapM_ (\(a, b) ->
    fmap (over both fromRational) [_point2 a, _point2 b] `linePlot`
      do  plotColor .= red
          lineStyle . _lw .= 1.5)
         
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
  -> QDiagram b V2 Double Any
textreportDiagram u n pq =
  let (p, q) =
        _point2 pq
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
      reporttext a b x =
        alignedText a b x # fontSizeL 6 # dejavuSansMono # fc red
  in  vcat' (with & sep .~ 15)
        [
            reporttext (0.650) (-1.65) ("Moment              " <> textRational (p * 1000) <> " pound/inches")
          , reporttext (0.805) (-2.20) ("All Up Weight       " <> textRational q <> " pounds")
          , reporttext (1.245) (-2.20) ("Utility Category    " <> textPointLocationResult utility)
          , reporttext (1.190) (-2.80) ("Normal Category     " <> textPointLocationResult normal)
        ]
 
plotMomentDiagram :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  SimplePolygon () Rational
  -> SimplePolygon () Rational
  -> [(Point 2 Rational, Point 2 Rational)]
  -> QDiagram b V2 Double Any
plotMomentDiagram u n x =
  let r = r2Axis &~ 
            do  plotgrid
                plotenvelope [u, n]
                plotlines x
  in  renderAxis r # centerX # dejavuSansMono

momentDiagram ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  SimplePolygon () Rational
  -> SimplePolygon () Rational
  -> Point 2 Rational
  -> QDiagram b V2 Double Any
momentDiagram u n pq =
  vcat' (with & sep .~ 15)
    [
      plotMomentDiagram u n (plotmomentpoint pq)
    , textreportDiagram u n pq
    ]

momentDiagramFuelline :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  SimplePolygon () Rational
  -> SimplePolygon () Rational
  -> Point 2 Rational
  -> Point 2 Rational
  -> QDiagram b V2 Double Any
momentDiagramFuelline u n a b =
  plotMomentDiagram u n [(a, b)]

makepoly ::
  [(r, r)]
  -> SimplePolygon () r
makepoly = 
  fromPoints . fmap (ext . uncurry point2)