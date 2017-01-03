{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeUpper(
  MeasuredArmRangeUpper
, HasMeasuredArmRangeUpper(..)
, HasMeasuredArmRangeUppers(..)
, SetMeasuredArmRangeUpper(..)
) where

import Control.Category((.))
import Control.Lens(Traversal', Setter', makeClassy, iso)
import Data.Aviation.Units(Inches(inches), Centimetres(centimetres))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Ratio((%))
import Numeric.Lens(dividing)
import Prelude(Show, Rational)

newtype MeasuredArmRangeUpper =
  MeasuredArmRangeUpper
    Rational
  deriving (Eq, Ord, Show)

makeClassy ''MeasuredArmRangeUpper

class HasMeasuredArmRangeUppers a where
  measuredArmRangeUppers ::
    Traversal'
      a
      MeasuredArmRangeUpper

instance HasMeasuredArmRangeUppers MeasuredArmRangeUpper where
  measuredArmRangeUppers =
    measuredArmRangeUpper

class SetMeasuredArmRangeUpper a where
  setMeasuredArmRangeUpper ::
    Setter'
      a
      MeasuredArmRangeUpper

instance SetMeasuredArmRangeUpper MeasuredArmRangeUpper where
  setMeasuredArmRangeUpper =
    measuredArmRangeUpper

instance Inches MeasuredArmRangeUpper where
  inches =
    iso
      MeasuredArmRangeUpper
      (\(MeasuredArmRangeUpper x) -> x)

instance Centimetres MeasuredArmRangeUpper where
  centimetres =
    let rate = 254 % 100
    in  dividing rate . inches
    