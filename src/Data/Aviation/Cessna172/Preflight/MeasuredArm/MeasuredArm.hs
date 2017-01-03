{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArm(
  MeasuredArm(..)
, staticMeasuredArm
, rangeMeasuredArm
, HasMeasuredArm(..)
, HasMeasuredArms(..)
, SetMeasuredArm(..)
, HasMeasuredArm0(..)
) where

import Control.Category((.))
import Control.Lens(Lens', Traversal', Setter', lens, makeClassy)
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeLower(HasMeasuredArmRangeLowers(measuredArmRangeLowers), SetMeasuredArmRangeLower(setMeasuredArmRangeLower))
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRange(MeasuredArmRange, HasMeasuredArmRanges(measuredArmRanges), SetMeasuredArmRange(setMeasuredArmRange), HasMeasuredArmRange0(measuredArmRange0))
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmStatic(MeasuredArmStatic, HasMeasuredArmStatic(measuredArmStatic), HasMeasuredArmStatics(measuredArmStatics), SetMeasuredArmStatic(setMeasuredArmStatic))
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmRangeUpper(HasMeasuredArmRangeUppers(measuredArmRangeUppers), SetMeasuredArmRangeUpper(setMeasuredArmRangeUpper))
import Data.Functor((<$>))
import Data.Traversable(traverse)
import Data.Eq(Eq)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Prelude(Show)
    
data MeasuredArm =
  MeasuredArm
    MeasuredArmStatic
    (Maybe MeasuredArmRange)
  deriving (Eq, Ord, Show)

makeClassy ''MeasuredArm

staticMeasuredArm ::
  MeasuredArmStatic
  -> MeasuredArm
staticMeasuredArm x =
  MeasuredArm x Nothing

rangeMeasuredArm ::
  MeasuredArmStatic
  -> MeasuredArmRange
  -> MeasuredArm
rangeMeasuredArm x =
  MeasuredArm x . Just

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

instance SetMeasuredArmStatic MeasuredArm where
  setMeasuredArmStatic =
    measuredArmStatic . setMeasuredArmStatic

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

class HasMeasuredArm0 a where
  measuredArm0 ::
    Lens'
      a
      (Maybe MeasuredArm)

instance HasMeasuredArmRange0 MeasuredArm where
  measuredArmRange0 =
    lens
      (\(MeasuredArm _ r) -> r)
      (\(MeasuredArm s _) r -> MeasuredArm s r)
