{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Probability where

import HyloDP.Semiring

newtype Probability = Probability Double deriving(Show, Eq, Ord, Fractional, Num)

instance Semiring Probability where
  (<+>) = (+)
  (<.>) = (*)
  zero = 0
  one = 1

instance Bounded Probability where
  maxBound = 1
  minBound = 0


