{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeUpper(
  MeasuredArmRangeUpper
, HasMeasuredArmRangeUpper(..)
, HasMeasuredArmRangeUppers(..)
, SetMeasuredArmRangeUpper(..)
, HasMeasuredArmRangeUpper0(..)
) where

import Control.Category((.))
import Control.Lens(Lens', Traversal', Setter', makeClassy, iso)
import Data.Aviation.Units(Inches(inches), Centimetres(centimetres))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Data.Ratio((%))
import Data.Semigroup(Semigroup((<>)))
import Numeric.Lens(dividing)
import Prelude(Show, Rational, (+))

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

class HasMeasuredArmRangeUpper0 a where
  measuredArmRangeUpper0 ::
    Lens'
      a
      (Maybe MeasuredArmRangeUpper)

instance Inches MeasuredArmRangeUpper where
  inches =
    iso
      MeasuredArmRangeUpper
      (\(MeasuredArmRangeUpper x) -> x)

instance Centimetres MeasuredArmRangeUpper where
  centimetres =
    dividing (254 % 100) . inches
    
instance Semigroup MeasuredArmRangeUpper where
  (<>) =
    mappend

instance Monoid MeasuredArmRangeUpper where
  mempty =
    MeasuredArmRangeUpper 0
  MeasuredArmRangeUpper w1 `mappend` MeasuredArmRangeUpper w2 =
    MeasuredArmRangeUpper (w1 + w2)
