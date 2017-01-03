{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aviation.Units.Poundinches(
  ToPoundinches(..)
) where

import Control.Category(id)
import Control.Lens(Getter, Iso', view, review, to)
import Data.Aviation.Units.Pounds(Pounds(pounds))
import Data.Aviation.Units.Inches(Inches(inches))
import Prelude(Rational, (*))

class ToPoundinches a where
  poundinches ::
    Getter a Rational

instance ToPoundinches Rational where
  poundinches =
    id

instance (Pounds p, Inches i) => ToPoundinches (p, i) where
  poundinches =
    to (\(p, i) -> let p' = review pounds p
                       i' = review inches i
                   in  p' * i')