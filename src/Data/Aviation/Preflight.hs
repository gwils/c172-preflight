{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Aviation.Preflight where

import Control.Category((.))
import Control.Lens((^.))
import Diagrams.Prelude(V2(V2), mkSizeSpec)
import Data.Foldable(mapM_)
import Data.Maybe(Maybe(Just))
import Data.Semigroup((<>))
import Data.Aviation.C172.Diagrams
import Data.Aviation.C172.WB
import Data.Aviation.Units
import Data.Aviation.WB
import Data.String(String)
import System.IO(IO)

tonymorris = 80 ^. kilograms
george = 85 ^. kilograms
jessica = 55 ^. kilograms
joshua = 64 ^. kilograms

vhlseBEW :: Weight
vhlseBEW = 1691.6 ^. pounds

vhlseArms :: C172AircraftArms Arm
vhlseArms = bewC172AircraftArms (40.6 ^. inches)

vhvvoBEW :: Weight
vhvvoBEW = 1684.3 ^. pounds

vhvvoArms :: C172AircraftArms Arm
vhvvoArms = bewC172AircraftArms (40.6 ^. inches)

flight20170131Weight ::
  C172Arms Weight
flight20170131Weight =
  C172Arms
    (tonymorris <> george)
    (jessica <> joshua)
    (40 ^. usgallonsV . avgas100LL)
    (30 ^. kilograms)
    (10 ^. kilograms)

flightMoments ::
  [(String, Weight, C172Arms Weight, C172AircraftArms Arm, String)]
flightMoments =
  [
    (
      "20170131 Flight VH-VVO SIMULATION"
    , vhvvoBEW
    , flight20170131Weight
    , vhvvoArms
    , "dist/flight20170131"
    )
  ]

main ::
  IO ()
main =
  mapM_ (\(s, w, ws, a, o) ->
    renderMomentDiagrams s (totalC172Moment w ws a) (mkSizeSpec (V2 (Just 800) (Just 1131.2))) o) flightMoments
