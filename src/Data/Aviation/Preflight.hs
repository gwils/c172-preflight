{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Aviation.Preflight where

import Control.Category((.))
import Control.Lens((^.))
import Diagrams.Prelude(V2(V2), mkSizeSpec)
import Data.Foldable(mapM_)
import Data.Maybe(Maybe(Just))
import Data.Monoid(mempty)
import Data.Semigroup((<>))
import Data.Aviation.C172.Diagrams
import Data.Aviation.C172.WB
import Data.Aviation.Units
import Data.Aviation.WB
import Data.String(String)
import System.IO(IO)

tonymorris = 80 ^. kilograms
georgewilson = 85 ^. kilograms
jessicaatherton = 55 ^. kilograms
joshuamorris = 64 ^. kilograms
amandaward = 100 ^. kilograms
adammorris = 55 ^. kilograms

vhlseBEW = 1691.6 ^. pounds
vhlseArms = bewC172AircraftArms (40.6 ^. inches)

-- vhafrBEW = 1684.3 ^. pounds
-- vhafrArms = 39.37 ^. inches

flight20170102Weight ::
  C172Arms Weight
flight20170102Weight =
  C172Arms
    (tonymorris <> georgewilson)
    jessicaatherton
    (40 ^. usgallonsV . avgas100LL)
    (10 ^. kilograms)
    mempty

flight20170121Weight ::
  C172Arms Weight
flight20170121Weight =
  C172Arms
    (tonymorris <> amandaward)
    (adammorris <> joshuamorris)
    (30 ^. usgallonsV . avgas100LL)
    (10 ^. kilograms)
    mempty

flightMoments ::
  [(String, Weight, C172Arms Weight, C172AircraftArms Arm, String)]
flightMoments =
  [
    (
      "20170102 Flight VH-LSE PAX: George, Jess"
    , vhlseBEW
    , flight20170102Weight
    , vhlseArms
    , "dist/flight20170102"
    )
  , (
      "20170121 VH-LSE PAX: Amanda, Joshua, Adam"
    , vhlseBEW
    , flight20170121Weight
    , vhlseArms
    , "dist/flight20170121"
    )
  ]

main ::
  IO ()
main =
  mapM_ (\(s, w, ws, a, o) ->
    renderMomentDiagrams s (totalC172Moment w ws a) (mkSizeSpec (V2 (Just 800) (Just 1131.2))) o) flightMoments
