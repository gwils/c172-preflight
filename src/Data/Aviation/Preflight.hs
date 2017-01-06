{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Aviation.Preflight where

import Control.Category((.))
import Control.Lens((^.))
import Diagrams.Prelude(V2(V2), mkSizeSpec)
import Data.Foldable(fold)
import Data.Maybe(Maybe(Just))
import Data.Monoid(mempty)
import Data.Semigroup((<>))
import Data.Aviation.C172
import Data.Aviation.Units
import Data.Aviation.WB
import System.IO(IO)

tony = 80 ^. kilograms
george = 85 ^. kilograms
jess = 55 ^. kilograms

dynamicWeights ::
  C172Arms Weight
dynamicWeights =
  C172Arms
    (tony <> george)
    jess
    (fold [30 ^. usgallonsV . avgas100LL, 15 ^. litresV . avgas100LL, 2 ^. imperialgallonsV . avgas100LL, 12 ^. pounds])
    (10 ^. kilograms)
    mempty

vhlseBEW = 1691.6 ^. pounds
vhlseArms = bewC172AircraftArms (40.6 ^. inches)
testFlightMoment = totalC172Moment vhlseBEW dynamicWeights vhlseArms

main ::
  IO ()
main =
  renderMomentDiagrams
    "20170102 Test Flight VH-LSE PAX: George, Jess"
    testFlightMoment
    (mkSizeSpec (V2 (Just 3200) (Just 4524.8)))
    "dist/output"


-- vhafrBEW = 1684.3 ^. pounds
-- vhafrArms = 39.37 ^. inches
