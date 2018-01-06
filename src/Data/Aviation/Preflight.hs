{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Aviation.Preflight where

import Control.Category((.))
import Control.Lens((^.))
import Data.Monoid(mempty)
import Data.Semigroup((<>))
import Data.Aviation.C172.WB
import Data.Aviation.Units(kilograms, inches, pounds)
import Data.Aviation.WB
import Data.String(String)

tonymorris = 80 ^. kilograms
georgewilson = 85 ^. kilograms
jessicaatherton = 55 ^. kilograms
joshuamorris = 64 ^. kilograms
amandaward = 100 ^. kilograms
adammorris = 55 ^. kilograms
gregdavis = 70 ^. kilograms
robertdenney = 100 ^. kilograms
jackmason = 70 ^. kilograms
paulbarber = 90 ^. kilograms
maceybarber = 34 ^. kilograms
ethanbarber = 47 ^. kilograms
stephenwatson = 75 ^. kilograms
jedws = 75 ^. kilograms
julien = 75 ^. kilograms
craigwatt = 110 ^. kilograms
butch = 100 ^. kilograms
nickhamilton = 70 ^. kilograms
-- sarahvandyke = 60 ^. kilograms

vhkjrBEW = 1477.7 ^. pounds
-- moment = 667.344 (obtained over the phone)
vhkjrArms = bewC172AircraftArms (45.1 ^. inches)

vhlseBEW = 1691.6 ^. pounds
vhlseArms = bewC172AircraftArms (40.6 ^. inches)

vhafrBEW :: Weight
vhafrBEW = 1684.3 ^. pounds

vhafrArms :: C172AircraftArms Arm
vhafrArms = bewC172AircraftArms (40.6 ^. inches)

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

denneyDavisMasonVHLSE ::
  C172Arms Weight
denneyDavisMasonVHLSE =
  C172Arms
    (tonymorris <> robertdenney)
    (gregdavis <> jackmason)
    (24 ^. usgallonsV . avgas100LL)
    (5 ^. kilograms)
    mempty

denneyDavisMasonVHAFR ::
  C172Arms Weight
denneyDavisMasonVHAFR =
  C172Arms
    (tonymorris <> robertdenney)
    (gregdavis <> jackmason)
    (26 ^. usgallonsV . avgas100LL)
    (5 ^. kilograms)
    mempty

flight20170820Weight ::
  C172Arms Weight
flight20170820Weight =
  C172Arms
    (tonymorris <> paulbarber)
    (maceybarber <> ethanbarber)
    (160 ^. litresV . avgas100LL)
    (10 ^. kilograms)
    (5 ^. kilograms)

dualflightKJR ::
  C172Arms Weight
dualflightKJR =
  C172Arms
    (tonymorris <> stephenwatson)
    mempty
    (54 ^. usgallonsV . avgas100LL)
    (10 ^. kilograms)
    mempty

soloflightKJR ::
  C172Arms Weight
soloflightKJR =
  C172Arms
    tonymorris
    mempty
    (54 ^. usgallonsV . avgas100LL)
    (10 ^. kilograms)
    mempty

flight20171204Weight ::
  C172Arms Weight
flight20171204Weight =
  C172Arms
    (tonymorris <> jedws)
    (julien)
    (53 ^. usgallonsV . avgas100LL)
    (10 ^. kilograms)
    (5 ^. kilograms)

flight20171229Weight ::
  C172Arms Weight
flight20171229Weight =
  C172Arms
    (tonymorris <> butch)
    (craigwatt)
    (130 ^. litresV . avgas100LL)
    (9 ^. kilograms)
    mempty

flight20180107Weight ::
  C172Arms Weight
flight20180107Weight =
  C172Arms
    (tonymorris <> nickhamilton)
    mempty
    (56 ^. usgallonsV . avgas100LL)
    (5 ^. kilograms)
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
  , (
      "Hypothetical flight VH-LSE PAX: Robert Denney, Greg Davis, Jack Mason"
    , vhlseBEW
    , denneyDavisMasonVHLSE
    , vhlseArms
    , "dist/denneyDavisMasonVHLSE"
    )
  , (
      "Hypothetical flight VH-AFR PAX: Robert Denney, Greg Davis, Jack Mason"
    , vhafrBEW
    , denneyDavisMasonVHAFR
    , vhafrArms
    , "dist/denneyDavisMasonVHAFR"
    )
  , (
      "20170820 VH-LSE. Paul (front seat), Macey, Ethan. Fuel: 160L, BagA: 10kg, BagB: 5kg"
    , vhlseBEW
    , flight20170820Weight
    , vhlseArms
    , "dist/paulMaceyEthanVHLSE"
    )
  , (
      "20171123 VH-KJR. Dual flight with Stephen Watson with 10kg baggage(A)"
    , vhkjrBEW
    , dualflightKJR
    , vhkjrArms
    , "dist/dualflightKJR"
    )
  , (
      "20171124 VH-KJR. Solo flight with 10kg baggage(A)"
    , vhkjrBEW
    , soloflightKJR
    , vhkjrArms
    , "dist/soloflightKJR"
    )
  , (
      "20171204 VH-LSE. Flight with jedws and julien, 10kg baggage(A), 5kg baggage(B)"
    , vhlseBEW
    , flight20171204Weight
    , vhlseArms
    , "dist/flight20171204Weight"
    )
  , (
      "20171229 VH-LSE. Flight with Craig and Butch Watt, 15kg baggage(A), 0kg baggage(B)"
    , vhlseBEW
    , flight20171229Weight
    , vhlseArms
    , "dist/flight20171229Weight"
    )
  , (
      "20180107 VH-LSE. Flight with Nick and Sarah, 5kg baggage(A), 0kg baggage(B)"
    , vhlseBEW
    , flight20180107Weight
    , vhlseArms
    , "dist/flight20180107Weight"
    )
  ]
