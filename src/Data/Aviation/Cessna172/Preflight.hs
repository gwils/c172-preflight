{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aviation.Cessna172.Preflight where

import Prelude
import Control.Lens(view, over, both, (&~), (.=), (%=), (.~), (&))
import Data.Foldable(toList)
import Data.Monoid(Any)
import Diagrams.Attributes(lwO, _lw)
import Diagrams.Prelude(V2(V2), black, mkSizeSpec, (#))
import Diagrams.Backend.Cairo(Cairo(Cairo), OutputType(PDF))
import Diagrams.Backend.Cairo.Internal(Options(CairoOptions)) -- (CairoOptions(..))
import Diagrams.Combinators(sep)
import Diagrams.Core.Compile(renderDia)
import Diagrams.Core.Types(QDiagram, Renderable)
import Diagrams.Path(Path)
import Diagrams.TwoD.Combinators(vcat')
import Diagrams.TwoD.Text
import Diagrams.Util(with)
import Plots
import Data.CircularSeq(CSeq)
import Data.Ext(ext, _core)
import Data.Geometry.Point(point2, _point2)
import Data.Geometry.Polygon(Polygon, fromPoints, outerBoundary)
import Plots.Axis.Render(renderAxis)

polygonPoint2 ::
  Fractional b =>
  Polygon t extra Rational ->
  CSeq (b, b)
polygonPoint2 =
  fmap (over both fromRational . _point2 . _core) . view outerBoundary

plot :: 
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  Axis b V2 Double
plot =
  let linePlotPolygon x c l = (linePlot  . toList . polygonPoint2  $ x) $ 
        do  plotColor .= c
            lineStyle . _lw .= l
  in  r2Axis &~ do
        
        linePlotPolygon (fromPoints . map ext $ [ point2 61 1500, point2 89 2200, point2 82.5 2200, point2 68 1950, point2 52.5 1500]) black 0.7
        
        minorTicksFunction .= minorTicksHelper 10
        majorTicksStyle %= lwO 1.6
        minorGridLinesStyle %= lwO 0.3
        majorGridLinesStyle %= lwO 0.6
        tickLabelFunction .= atMajorTicks (show . (round :: Double -> Int))
        minorGridLines . visible .= True

renderResult ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  QDiagram b V2 Double Any
renderResult = 
  vcat' (with & sep .~ 5) [renderAxis plot, alignedText (-0.1) (1.0) "this is some text" # fontSizeL 12]

main ::
  IO ()
main =
  let pdfoptions = CairoOptions
                  "output.pdf"
                  (mkSizeSpec (V2 (Just 800) (Just 1131.2)))
                  PDF
                  False
  in  fst (renderDia Cairo pdfoptions renderResult)
