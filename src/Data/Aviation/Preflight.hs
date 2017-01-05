{-# OPTIONS_GHC -w #-}

module Data.Aviation.Preflight where

import Prelude
import Control.Lens((^.))
import Diagrams.Prelude(V2(V2), mkSizeSpec)
import Diagrams.Backend.Cairo(Cairo(Cairo), OutputType(PNG, PDF, PS, SVG))
import Diagrams.Backend.Cairo.Internal(Options(CairoOptions))
import Diagrams.Core.Compile(renderDia)
import Data.Semigroup((<>))
import Data.Aviation.C172
import Data.Aviation.Units
import Data.Aviation.WB

tony = 80 ^. kilograms
george = 85 ^. kilograms
jess = 55 ^. kilograms

dynamicWeights ::
  C172Arms Weight
dynamicWeights =
  C172Arms
    (tony <> george)
    jess
    (306 ^. pounds)
    (10 ^. kilograms)
    mempty

vhlseBEW = 1691.6 ^. pounds
vhlseArms = bewC172AircraftArms (40.6 ^. inches)
testFlightMoment = totalC172Moment vhlseBEW dynamicWeights vhlseArms

main ::
  IO ()
main =
  let size = mkSizeSpec (V2 (Just 800) (Just 1131.2))
      nosize = mkSizeSpec (V2 Nothing Nothing)
      outputs = [("png", size, PNG), ("ps", size, PS), ("pdf", size, PDF), ("svg", nosize, SVG)]
      momd = momentDiagram "20170102 Test Flight VH-LSE PAX: George, Jess" testFlightMoment
      render (e, s, t) = fst (renderDia Cairo (CairoOptions ("dist/output." ++ e) s t False) momd)
  in  mapM_ render outputs

-- vhafrBEW = 1684.3 ^. pounds
-- vhafrArms = 39.37 ^. inches
