{-# LANGUAGE NoImplicitPrelude #-}

module Data.Aviation.WB.Zerofuel(
  Zerofuel(zerofuel)
) where

class Zerofuel z where
  zerofuel ::
    z
    -> z
