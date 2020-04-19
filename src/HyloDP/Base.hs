{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HyloDP.Base (
     DPProblem(..),
     DPTypes(..),
     dpSolve
) where

import Data.List(foldl')
import Data.Maybe(maybeToList)
import Data.MemoTrie(HasTrie, memo)
import HyloDP.Semirings

-- Dynamic Programming Problem

data DPProblem p sc d = DPProblem { initial :: p
                                  , isTrivial :: p -> Bool
                                  , subproblems :: p -> [(sc, d, p)]
                                  }

-- DPF

data DPF sc p = Trivial | Children [(sc, p)]

instance Functor (DPF sc) where
  fmap _ Trivial = Trivial
  fmap f (Children cs) = Children [(sc, f p) | (sc, p) <- cs]

-- DPTypes (defines how to combine score and decision to obtain solution)

class DPTypes sc d sol where
    combine :: sc -> d -> sol

-- trivial instance if the solution is just the score
instance DPTypes sc d sc where
    combine = const

instance DPTypes sc d (TMin sc) where
    combine = const . TMin

instance DPTypes sc d (TMax sc) where
    combine = const . TMax

instance DPTypes sc d (MaxProd sc) where
    combine = const . MaxProd

instance DPTypes sc d Count where
    combine = const . const $ Count 1

instance DPTypes sc d sol => DPTypes sc d (BestSolution d sol) where
    combine sc d = BestSolution (Just [d]) (combine sc d)

instance DPTypes sc (Maybe d) sol => DPTypes sc (Maybe d) (BestSolution d sol) where
    combine sc d = BestSolution (Just $ maybeToList d) (combine sc d)

instance DPTypes sc d sol => DPTypes sc d (AllSolutions d sol) where
    combine sc d = AllSolutions [([d], combine sc d)]

instance DPTypes sc (Maybe d) sol => DPTypes sc (Maybe d) (AllSolutions d sol) where
    combine sc d = AllSolutions [(maybeToList d, combine sc d)]

instance (DPTypes sc d sol, DPTypes sc d sol') => DPTypes sc d (sol, sol') where
    combine sc d = (combine sc d, combine sc d)

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
dpSolve dp = hyloM solve build $ initial dp
  where build p | isTrivial dp p = Trivial
                | otherwise = Children [(combine sc d, sp) |
                                        (sc, d, sp) <- subproblems dp p]
        solve Trivial = one
        solve (Children sols) =
           foldl' (<+>) zero [ sc <.> sol | (sc, sol) <- sols ]
