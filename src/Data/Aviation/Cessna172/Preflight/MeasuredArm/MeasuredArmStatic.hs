{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmStatic(
  MeasuredArmStatic
, HasMeasuredArmStatic(..)
, HasMeasuredArmStatics(..)
, SetMeasuredArmStatic(..)
, HasMeasuredArmStatic0(..)
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

newtype MeasuredArmStatic =
  MeasuredArmStatic
    Rational
  deriving (Eq, Ord, Show, Num, Real, Fractional, RealFrac)

makeClassy ''MeasuredArmStatic

class HasMeasuredArmStatics a where
  measuredArmStatics ::
    Traversal'
      a
      MeasuredArmStatic

instance HasMeasuredArmStatics MeasuredArmStatic where
  measuredArmStatics =
    measuredArmStatic

class SetMeasuredArmStatic a where
  setMeasuredArmStatic ::
    Setter'
      a
      MeasuredArmStatic

instance SetMeasuredArmStatic MeasuredArmStatic where
  setMeasuredArmStatic =
    measuredArmStatic

class HasMeasuredArmStatic0 a where
  measuredArmStatic0 ::
    Lens'
      a
      (Maybe MeasuredArmStatic)

instance Inches MeasuredArmStatic where
  inches =
    iso
      MeasuredArmStatic
      (\(MeasuredArmStatic x) -> x)

instance Centimetres MeasuredArmStatic where
  centimetres =
    dividing (254 % 100) . inches
