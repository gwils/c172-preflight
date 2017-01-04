{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.C172.C172AircraftArms(
  C172AircraftArms(..)
, HasC172AircraftArms(..)
, c172ArmsAircraft
) where

import Control.Applicative(Applicative((<*>), pure))
import Control.Lens(makeClassy, set, lens)
import Data.Aviation.WB(ArmStatic)
import Data.Aviation.C172.C172Arms
import Data.Aviation.WB.Arm(Arm, staticArm)
import Data.Aviation.WB.Zerofuel(Zerofuel(zerofuel))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldr))
import Data.Functor(Functor(fmap), (<$>))
import Data.Monoid
import Data.Ord(Ord)
import Data.Traversable(Traversable(traverse))
import Prelude(Show)

data C172AircraftArms a =
  C172AircraftArms {
    _aircraftArm ::
      a
  , c172Arms_ ::
      C172Arms a
  }
  deriving (Eq, Ord, Show)

makeClassy ''C172AircraftArms

instance Monoid a => Zerofuel (C172AircraftArms a) where
  zerofuel =
    set fuel mempty

instance HasC172Arms (C172AircraftArms a) a where
  c172Arms =
    lens
      (\(C172AircraftArms _ c) -> c)
      (\(C172AircraftArms c _) a -> C172AircraftArms c a)

instance Functor C172AircraftArms where
  fmap k (C172AircraftArms c x) =
    C172AircraftArms (k c) (fmap k x)

instance Applicative C172AircraftArms where
  pure a =
    C172AircraftArms a (pure a)
  C172AircraftArms c1 c2 <*> C172AircraftArms x1 x2 =
    C172AircraftArms (c1 x1) (c2 <*> x2)

instance Foldable C172AircraftArms where
  foldr k z (C172AircraftArms c x) =
    k c (foldr k z x)

instance Traversable C172AircraftArms where
  traverse k (C172AircraftArms c x) =
    C172AircraftArms <$> k c <*> traverse k x

c172ArmsAircraft ::
  ArmStatic
  -> C172AircraftArms Arm
c172ArmsAircraft a =
  C172AircraftArms
    (staticArm a)
    c172ArmsPOH
