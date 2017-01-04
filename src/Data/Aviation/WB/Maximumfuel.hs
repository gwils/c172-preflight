{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.WB.Maximumfuel(
  Maximumfuel(maximumfuel)
) where

class Maximumfuel m where
  maximumfuel ::
    m
    -> m
