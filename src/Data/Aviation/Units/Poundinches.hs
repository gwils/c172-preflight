{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Units.Poundinches(
  ToPoundinches(..)
) where

import Control.Category(id)
import Control.Lens(Getter, Iso', view, review)
import Data.Aviation.Units.Pounds(Pounds(pounds))
import Data.Aviation.Units.Inches(Inches(inches))
import Prelude(Rational, (*))

class ToPoundinches a where
  poundinches ::
    Getter a Rational

instance ToPoundinches Rational where
  poundinches =
    id
