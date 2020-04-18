module Fibonacci where

import HyloDP

data Fib a = Base | Pair a a

instance Functor Fib where
  fmap _ Base = Base
  fmap f (Pair a b) = Pair (f a) (f b)

fibDPProblem :: Int -> DPProblem Int Integer ()
fibDPProblem n = DPProblem n (<= 2) (\n -> [(1, (), n-1), (1, (), n-2)])

fib :: Int -> Integer
fib n = dpSolve $ fibDPProblem n
