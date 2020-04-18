module HyloDP.Base (
     DPProblem(..),
     dpSolve
) where

import Data.List(foldl')
import Data.MemoTrie(HasTrie, memo)
import HyloDP.Semirings(Semiring(..), DPTypes(..))

-- Dynamic Programming Problem

data DPProblem p sc d = DPProblem { initial2 :: p
                                  , isTrivial2 :: p -> Bool
                                  , subproblems2 :: p -> [(sc, d, p)]
                                  }

-- DPF

data DPF sc p = Trivial | Children [(sc, p)]

instance Functor (DPF sc) where
  fmap _ Trivial = Trivial
  fmap f (Children cs) = Children [(sc, f p) | (sc, p) <- cs]

-- Hylomorphisms

type Coalgebra f p = p -> f p
type Algebra f s = f s -> s

hylo :: Functor f => Algebra f s -> Coalgebra f p -> p -> s
hylo alg coalg = h
     where h = alg . fmap h . coalg

-- Memoization

hyloM :: (Functor f, HasTrie s) => Algebra f t -> Coalgebra f s -> s -> t
hyloM alg coalg = h
     where h = memo $ alg . fmap h . coalg

-- dpSolve4

dpSolve :: (HasTrie p, Semiring sol, DPTypes sc d sol) => DPProblem p sc d -> sol
dpSolve dp = hyloM solve build $ initial2 dp
  where build p | isTrivial2 dp p = Trivial
                | otherwise = Children [(combine sc d, sp) |
                                        (sc, d, sp) <- subproblems2 dp p]
        solve Trivial = one
        solve (Children sols) =
           foldl' (<+>) zero [ sc <.> sol | (sc, sol) <- sols ]
