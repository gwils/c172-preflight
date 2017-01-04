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
import Control.Lens(review, (^.))
import Diagrams.Prelude(V2(V2), mkSizeSpec)
import Diagrams.Backend.Cairo(Cairo(Cairo), OutputType(PNG, PDF, PS, SVG))
import Diagrams.Backend.Cairo.Internal(Options(CairoOptions)) -- (CairoOptions(..))
import Diagrams.Core.Compile(renderDia)
import Data.Geometry.Point(Point, point2)
import Data.Geometry.Polygon(SimplePolygon)
import Data.Semigroup((<>))
import Data.Aviation.C172(C172Arms(C172Arms), C172AircraftArms(C172AircraftArms), c172ArmsAircraft)
import Data.Aviation.C172.C172MomentEnvelope
import Data.Aviation.C172.Diagrams
import Data.Aviation.Units(inches, pounds, kilograms, thouinches)
import Data.Aviation.WB(Arm, Moment(Moment), HasMoment, totalMoment, momentX, Weight)



----

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

totalMomentPoundInchesPoint ::
  (HasMoment moment, Foldable f) =>
  f moment
  -> Point 2 Rational
totalMomentPoundInchesPoint x =
  let Moment w a = totalMoment pounds inches x
  in  point2 (review thouinches a) (review pounds w)

c172sNormalCategoryPoly :: 
  SimplePolygon () Rational
c172sNormalCategoryPoly =
  makepoly c172sNormalCategory

c172sUtilityCategoryPoly :: 
  SimplePolygon () Rational
c172sUtilityCategoryPoly =
  makepoly c172UtilityCategory

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
  in  mapM_ (\o -> fst (renderDia Cairo o (momentDiagram c172sUtilityCategoryPoly c172sNormalCategoryPoly example))) [pngoptions, psoptions, pdfoptions, svgoptions]
