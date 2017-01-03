{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Cessna172.Preflight.Moment(
  Moment
, HasMoment(..)
, HasMoments(..)
, SetMoment(..)
, HasMoment0(..)
) where

import Control.Lens(Lens', Traversal', Setter', makeClassy, iso)
import Data.Aviation.Units.Poundinches(Poundinches(poundinches))
import Data.Eq(Eq)
import Data.Maybe(Maybe)
import Data.Monoid(Monoid)
import Data.Ord(Ord)
import Data.Semigroup
import Prelude(Show, Rational, (+))

newtype Moment =
  Moment
    Rational -- normalise to pound/inches
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
    Moment 0
  Moment w1 `mappend` Moment w2 =
    Moment (w1 + w2)

instance Poundinches Moment where
  poundinches =
    iso
      Moment
      (\(Moment x) -> x)
