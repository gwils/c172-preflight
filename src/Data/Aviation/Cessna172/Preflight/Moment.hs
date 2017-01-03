{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aviation.Cessna172.Preflight.Moment(
  Moment(..)
, HasMoment(..)
, HasMoments(..)
, SetMoment(..)
, HasMoment0(..)
, totalMoment
) where

import Control.Category((.))
import Control.Lens(Lens', Traversal', Setter', Iso', lens, makeClassy, review, view, to, re)
import Data.Aviation.Units.Poundinches(ToPoundinches(poundinches))
import Data.Aviation.Units.Pounds(pounds)
import Data.Aviation.Units.Inches(inches)
import Data.Aviation.Cessna172.Preflight.Arm.ArmStatic(ArmStatic, HasArmStatic(armStatic), HasArmStatics(armStatics), SetArmStatic(setArmStatic))
import Data.Aviation.Cessna172.Preflight.Weight(Weight, HasWeight(weight), HasWeights(weights), SetWeight(setWeight))
import Data.Eq(Eq)
import Data.Foldable(Foldable, foldl)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid)
import Data.Ord(Ord)
import Data.Semigroup
import Prelude(Show, (*), (+), Rational)

data Moment =
  Moment
    Weight
    ArmStatic
  deriving (Eq, Ord, Show)

makeClassy ''Moment

class HasMoments a where
  moments ::
    Traversal'
      a
      Moment

instance HasMoments Moment where
  moments =
    moment

class SetMoment a where
  setMoment ::
    Setter'
      a
      Moment

instance SetMoment Moment where
  setMoment =
    moment

class HasMoment0 a where
  moment0 ::
    Lens'
      a
      (Maybe Moment)

instance Semigroup Moment where
  (<>) =
    mappend

instance Monoid Moment where
  mempty =
    Moment mempty mempty
  Moment w1 a1 `mappend` Moment w2 a2 =
    Moment (w1 `mappend` w2) (a1 `mappend` a2)

instance ToPoundinches Moment where
  poundinches =
    to (\(Moment w a) -> 
      let w' = review pounds w
          a' = review inches a
      in  w' * a')

instance HasWeight Moment where
  weight =
    lens
      (\(Moment w _) -> w)
      (\(Moment _ a) w -> Moment w a)

instance HasWeights Moment where
  weights =
    weight

instance SetWeight Moment where
  setWeight =
    weight

instance HasArmStatic Moment where
  armStatic =
    lens
      (\(Moment _ a) -> a)
      (\(Moment w _) a -> Moment w a)

instance HasArmStatics Moment where
  armStatics =
    armStatic

instance SetArmStatic Moment where
  setArmStatic =
    armStatic

totalMoment ::
  (HasMoment moment, Foldable f) =>
  Iso' Rational Weight
  -> Iso' Rational ArmStatic
  -> f moment
  -> Moment
totalMoment w a m =
  let (mw, ma) = foldl (\(w', a') n ->  let w'' = view (moment . weight . re w) n
                                            a'' = view (moment . armStatic . re a) n
                                        in  (w' + w'', a' + w'' * a'')) (0, 0) m
  in  Moment (view w mw) (view a ma)
             