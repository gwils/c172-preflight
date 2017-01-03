{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRange(
  MeasuredArmRange
, HasMeasuredArmRange(..)
, HasMeasuredArmRanges(..)
, SetMeasuredArmRange(..)
, HasMeasuredArmRange0(..)
) where

import Control.Lens(Lens', Traversal', Setter', lens, makeClassy)
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeLower(MeasuredArmRangeLower, HasMeasuredArmRangeLower(measuredArmRangeLower), HasMeasuredArmRangeLowers(measuredArmRangeLowers), SetMeasuredArmRangeLower(setMeasuredArmRangeLower))
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeUpper(MeasuredArmRangeUpper, HasMeasuredArmRangeUpper(measuredArmRangeUpper), HasMeasuredArmRangeUppers(measuredArmRangeUppers), SetMeasuredArmRangeUpper(setMeasuredArmRangeUpper))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Ord(Ord)
import Prelude(Show)

data MeasuredArmRange =
  MeasuredArmRange
    MeasuredArmRangeLower
    MeasuredArmRangeUpper
  deriving (Eq, Ord, Show)

makeClassy ''MeasuredArmRange

class HasMeasuredArmRanges a where
  measuredArmRanges ::
    Traversal'
      a
      MeasuredArmRange

instance HasMeasuredArmRanges MeasuredArmRange where
  measuredArmRanges =
    measuredArmRange

class SetMeasuredArmRange a where
  setMeasuredArmRange ::
    Setter'
      a
      MeasuredArmRange

instance SetMeasuredArmRange MeasuredArmRange where
  setMeasuredArmRange =
    measuredArmRange

instance HasMeasuredArmRangeLower MeasuredArmRange where
  measuredArmRangeLower =
    lens
      (\(MeasuredArmRange lower _) -> lower)
      (\(MeasuredArmRange _ upper) lower -> MeasuredArmRange lower upper)

instance HasMeasuredArmRangeUpper MeasuredArmRange where
  measuredArmRangeUpper =
    lens
      (\(MeasuredArmRange _ upper) -> upper)
      (\(MeasuredArmRange lower _) upper -> MeasuredArmRange lower upper)

instance HasMeasuredArmRangeLowers MeasuredArmRange where
  measuredArmRangeLowers =
    measuredArmRangeLower
    
instance HasMeasuredArmRangeUppers MeasuredArmRange where
  measuredArmRangeUppers =
    measuredArmRangeUpper

instance SetMeasuredArmRangeLower MeasuredArmRange where
  setMeasuredArmRangeLower =
    measuredArmRangeLower
    
instance SetMeasuredArmRangeUpper MeasuredArmRange where
  setMeasuredArmRangeUpper =
    measuredArmRangeUpper

class HasMeasuredArmRange0 a where
  measuredArmRange0 ::
    Lens'
      a
      (Maybe MeasuredArmRange)
