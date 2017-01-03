{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord)
import Data.Ratio((%))
import Data.Semigroup(Semigroup((<>)))
import Numeric.Lens(dividing)
import Prelude(Show, Rational, (+))

newtype MeasuredArmStatic =
  MeasuredArmStatic
    Rational
  deriving (Eq, Ord, Show)

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

instance Semigroup MeasuredArmStatic where
  (<>) =
    mappend

instance Monoid MeasuredArmStatic where
  mempty =
    MeasuredArmStatic 0
  MeasuredArmStatic w1 `mappend` MeasuredArmStatic w2 =
    MeasuredArmStatic (w1 + w2)
