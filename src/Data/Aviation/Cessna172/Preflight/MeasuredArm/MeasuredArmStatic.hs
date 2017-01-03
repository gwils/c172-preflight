{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmStatic(
  MeasuredArmStatic
, HasMeasuredArmStatic(..)
, HasMeasuredArmStatics(..)
, SetMeasuredArmStatic(..)
) where

import Control.Category((.))
import Control.Lens(Traversal', Setter', makeClassy, iso)
import Data.Aviation.Units(Inches(inches), Centimetres(centimetres))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Ratio((%))
import Numeric.Lens(dividing)
import Prelude(Show, Rational)

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

instance Inches MeasuredArmStatic where
  inches =
    iso
      MeasuredArmStatic
      (\(MeasuredArmStatic x) -> x)

instance Centimetres MeasuredArmStatic where
  centimetres =
    let rate = 254 % 100
    in  dividing rate . inches
