{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module: HyloDP.Base
Description: DP solver aproach using hylomorphims with memoization.
Copyright: (c) David Llorens and Juan Miguel Vilar, 2020
License: BSD-3-Clause
Stability: experimental

This module contains a DP problem solver. The solver input is an instance of
the class 'DPProblem'. To create this instance the problem must be described
in terms of how its instances are decomposed into smaller ones.
-}
module HyloDP.Base (
     -- ** The Dynamic Programing Problem (DPProblem) type
     DPProblem(..),
     DPTypes(..),
     -- ** The Dynamic Programing solver
     dpSolve
) where

import Data.List(foldl')
import Data.Maybe(maybeToList)
import Data.MemoTrie(HasTrie, memo)
import HyloDP.Semirings

{-|
A wrapper type over the problem type that includes how to decompose it. Used types:

     [@p@]: is the type of the problem.
     [@sc@]: is the type of the scores.
     [@d@]: is the type of the decisions.
-}

data DPProblem p sc d = DPProblem
     { initial :: p                     -- ^ The current problem
     , isTrivial :: p -> Bool           -- ^ To check if the current problem is a trivial (non-decomposable) problem
     , subproblems :: p -> [(sc, d, p)] -- ^ For every valid decision (and its score) builds a tuple (score, decision, subproblem)
     }

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

-- | The type used for decomposing the problem into subproblems
type Coalgebra f p = p -> f p

-- | The type used for componing the solution to the subproblems. f is a Functor
type Algebra f s = f s -> s

-- | The hylomorphism implementation (not used)
hylo :: Functor f => Algebra f s -> Coalgebra f p -> p -> s
hylo alg coalg = h
     where h = alg . fmap h . coalg

-- | The hylomorphism implementation with memoization
hyloM :: (Functor f, HasTrie s) => Algebra f t -> Coalgebra f s -> s -> t
hyloM alg coalg = h
     where h = memo $ alg . fmap h . coalg

-- | The 'Functor' needed by our algebra and coalgebra
data DPF sc p = Trivial | Children [(sc, p)]

-- | The Functor instance
instance Functor (DPF sc) where
  fmap _ Trivial = Trivial
  fmap f (Children cs) = Children [(sc, f p) | (sc, p) <- cs]

-- | The generic Dynamic Programing solver. It takes a 'DPProblem' and return its solution.
-- With the constraint 'HasTrie p' the problem type 'p' can be memoized.
-- With the constraint 'Semiring sol' the result type is a instance of 'Semiring'.
-- With the constraint 'DPTypes sc d sol' it is possible to combine a score and a decision to obtain a solution.
dpSolve :: ( HasTrie p, Semiring sol, DPTypes sc d sol)
     => DPProblem p sc d -- ^ The problem wrapped inside a 'DPProblem'.
     -> sol              -- ^ The solution to the problem. It's a 'Semiring' type.
dpSolve dp = hyloM solve build $ initial dp
  where build p | isTrivial dp p = Trivial
                | otherwise = Children [(combine sc d, sp) |
                                        (sc, d, sp) <- subproblems dp p]
        solve Trivial = one
        solve (Children sols) =
           foldl' (<+>) zero [ sc <.> sol | (sc, sol) <- sols ]
