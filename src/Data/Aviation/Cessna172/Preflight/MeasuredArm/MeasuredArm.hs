{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArm(
  MeasuredArm
, HasMeasuredArm(..)
, HasMeasuredArms(..)
, SetMeasuredArm(..)
) where

import Control.Category((.))
import Control.Lens(Traversal', Setter', lens, makeClassy)
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeLower
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRange
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmStatic
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeUpper
import Data.Functor((<$>))
import Data.Traversable(traverse)
import Data.Eq(Eq)
import Data.Maybe
import Data.Ord(Ord)
import Prelude(Show)
    
data MeasuredArm =
  MeasuredArm
    MeasuredArmStatic -- normalised to inches
    (Maybe MeasuredArmRange)
  deriving (Eq, Ord, Show)

makeClassy ''MeasuredArm

class HasMeasuredArms a where
  measuredArms ::
    Traversal'
      a
      MeasuredArm

instance HasMeasuredArms MeasuredArm where
  measuredArms =
    measuredArm

class SetMeasuredArm a where
  setMeasuredArm ::
    Setter'
      a
      MeasuredArm

instance SetMeasuredArm MeasuredArm where
  setMeasuredArm =
    measuredArm

instance HasMeasuredArmStatic MeasuredArm where
  measuredArmStatic =
    lens
      (\(MeasuredArm s _) -> s)
      (\(MeasuredArm _ r) s -> MeasuredArm s r)

instance HasMeasuredArmStatics MeasuredArm where
  measuredArmStatics =
    measuredArmStatic
    
instance HasMeasuredArmRanges MeasuredArm where
  measuredArmRanges f (MeasuredArm s r) =
    MeasuredArm s <$> traverse f r

instance SetMeasuredArmRange MeasuredArm where
  setMeasuredArmRange =
    measuredArmRanges

instance HasMeasuredArmRangeLowers MeasuredArm where
  measuredArmRangeLowers =
    measuredArmRanges . measuredArmRangeLowers

instance HasMeasuredArmRangeUppers MeasuredArm where
  measuredArmRangeUppers =
    measuredArmRanges . measuredArmRangeUppers

instance SetMeasuredArmRangeLower MeasuredArm where
  setMeasuredArmRangeLower =
    setMeasuredArmRange . setMeasuredArmRangeLower

instance SetMeasuredArmRangeUpper MeasuredArm where
  setMeasuredArmRangeUpper =
    setMeasuredArmRange . setMeasuredArmRangeUpper
