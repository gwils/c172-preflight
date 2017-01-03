{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Cessna172.Preflight.Moment(
  Moment(..)
, HasMoment(..)
, HasMoments(..)
, SetMoment(..)
, HasMoment0(..)
) where

import Control.Lens(Lens', Traversal', Setter', makeClassy, review, to)
import Data.Aviation.Units.Poundinches(ToPoundinches(poundinches))
import Data.Aviation.Units.Pounds(pounds)
import Data.Aviation.Units.Inches(inches)
import Data.Aviation.Cessna172.Preflight.Weight(Weight)
import Data.Aviation.Cessna172.Preflight.MeasuredArm.MeasuredArmStatic(MeasuredArmStatic)
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid)
import Data.Ord(Ord)
import Data.Semigroup
import Prelude(Show, (*))

data Moment =
  Moment
    Weight
    MeasuredArmStatic
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
