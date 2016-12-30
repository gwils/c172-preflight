{-# LANGUAGE FlexibleContexts #-}

module Data.Aviation.Cessna172.Preflight where

import Prelude
import Control.Lens
import Data.Monoid(Any)
import Diagrams.Prelude
import Diagrams.Backend.Cairo(Cairo(Cairo), OutputType(PDF))
import Diagrams.Backend.Cairo.Internal(Options(CairoOptions)) -- (CairoOptions(..))
import Diagrams.Combinators(sep)
import Diagrams.Core.Compile(renderDia)
import Diagrams.Core.Types(QDiagram, Renderable)
import Diagrams.Path(Path)
import Diagrams.TwoD.Combinators(vcat')
import Diagrams.TwoD.Ellipse(circle)
import Diagrams.TwoD.Text(Text, text)
import Diagrams.Util(with)
import Plots
import Plots.Axis.Render(renderAxis)

renderResult ::
  (Renderable (Text Double) b, Renderable (Path V2 Double) b) =>
  QDiagram b V2 Double Any
renderResult = 
  vcat' (with & sep .~ 5) [renderAxis r2Axis, circle 20, text "this is some text"]
      

main ::
  IO ()
main =
  let pdfoptions = CairoOptions
                  "output.pdf"
                  (mkSizeSpec (V2 (Just 800) Nothing))
                  PDF
                  False      
  in  fst (renderDia Cairo pdfoptions renderResult)
