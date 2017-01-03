{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Units.Poundinches(
  Poundinches(poundinches)
, productPoundinches
) where

import Control.Category(id)
import Control.Lens(Iso', view, review)
import Data.Aviation.Units.Pounds(Pounds(pounds))
import Data.Aviation.Units.Inches(Inches(inches))
import Prelude(Rational, (*))

class Poundinches a where
  poundinches ::
    Iso' Rational a

instance Poundinches Rational where
  poundinches =
    id

productPoundinches ::
  (Inches i, Pounds p, Poundinches pi) =>
  p
  -> i
  -> pi
productPoundinches p i =
  let p' = review pounds p
      i' = review inches i
  in view poundinches (p' * i')
