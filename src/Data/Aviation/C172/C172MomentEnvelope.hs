{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.C172.C172MomentEnvelope(
  c172rNormalCategory
, c172sNormalCategory
) where

import Prelude(Fractional)

c172rNormalCategory ::
  Fractional a =>
  [(a, a)]
c172rNormalCategory =
  c172NormalCategory
    (98, 2450)

c172sNormalCategory ::
  Fractional a =>
  [(a, a)]
c172sNormalCategory =
  c172NormalCategory
    (120.5, 2550)

----

-- not exported


c172NormalCategory ::
  Fractional a =>
  (a, a)
  -> [(a, a)]
c172NormalCategory x =
  [
      x
    , (71, 1500)
    , (61, 1500)
    , (89, 2200)
    , (82.5, 2200)
    , (104.5, 2550)
  ]
