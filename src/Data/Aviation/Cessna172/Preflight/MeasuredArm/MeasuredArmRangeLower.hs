{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeLower(
  MeasuredArmRangeLower
, HasMeasuredArmRangeLower(..)
, HasMeasuredArmRangeLowers(..)
, SetMeasuredArmRangeLower(..)
, HasMeasuredArmRangeLower0(..)
) where

import Control.Category((.))
import Control.Lens(Lens', Traversal', Setter', makeClassy, iso)
import Data.Aviation.Units(Inches(inches), Centimetres(centimetres))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Data.Ratio((%))
import Numeric.Lens(dividing)
import Prelude(Show, Rational, Num, Real, Fractional, RealFrac)

newtype MeasuredArmRangeLower =
  MeasuredArmRangeLower
    Rational
  deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac)

makeClassy ''MeasuredArmRangeLower

class HasMeasuredArmRangeLowers a where
  measuredArmRangeLowers ::
    Traversal'
      a
      MeasuredArmRangeLower

instance HasMeasuredArmRangeLowers MeasuredArmRangeLower where
  measuredArmRangeLowers =
    measuredArmRangeLower

class SetMeasuredArmRangeLower a where
  setMeasuredArmRangeLower ::
    Setter'
      a
      MeasuredArmRangeLower

instance SetMeasuredArmRangeLower MeasuredArmRangeLower where
  setMeasuredArmRangeLower =
    measuredArmRangeLower

class HasMeasuredArmRangeLower0 a where
  measuredArmRangeLower0 ::
    Lens'
      a
      (Maybe MeasuredArmRangeLower)

instance Inches MeasuredArmRangeLower where
  inches =
    iso
      MeasuredArmRangeLower
      (\(MeasuredArmRangeLower x) -> x)

instance Centimetres MeasuredArmRangeLower where
  centimetres =
    dividing (254 % 100) . inches
