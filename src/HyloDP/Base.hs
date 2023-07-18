{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

{-|
Module: HyloDP.Base
Description: A solver for Dynamic Programming problems
Copyright: (c) David Llorens and Juan Miguel Vilar, 2020
License: BSD-3-Clause
Stability: experimental

This module implements the DP problem solver. Its input is an instance
of the type 'DPProblem'. This type holds two funcions:

* @isTrivial@ that is true when an instance can be trivially solved.

* @subproblems@ that decompose the instance in smaller subproblems.

It also holds @initial@, the initial instance of the problem, the one
that you want to solve.

An example is the problem of finding the longest common
subsequence of two lists (ie, the LCS of @xs@ and @ys@ is a list all whose
elements appear both in @xs@ and @ys@ in the same order):

* If both @xs@ and @ys@ are empty, the problem is trivial.

* If not, check the heads of @xs@ and @ys@. If they are
  equal, take it and find the lcs of the tails. If they are different,
  don't take the element and consider one list and the tail of the
  other.

In code:


> import HyloDP
>
> lcsDPProblem :: Eq a => [a] -> [a] -> DPProblem ([a], [a]) Int (Maybe a)
> lcsDPProblem xs ys = DPProblem (xs, ys) isTrivial subproblems
>   where isTrivial (xs, ys) = null xs || null ys
>         subproblems (l@(x:xs), r@(y:ys))
>           | x == y = [(1, Just x, (xs, ys))]
>           | otherwise = [ (0, Nothing, (xs, r))
>                         , (0, Nothing, (l, ys))
>                         ]
>


We use 'Nothing' to signal that the element is dropped and 'Just x' to
signal that it is taken. Also, when we take an element, the score for
that decision is one, while dropping the element scores zero.

Now, you can find the number of chars in common between @"train"@ and
@"raising"@ like this:


> print (dpSolve $ lcsDPProblem "train" "raising" :: TMax Int)

But you are probably more interesting in the best solution, so you can
do

> print (dpSolve $ lcsDPProblem "train" "raising" :: BestSolution Char (TMax Int))

As you can see, by choosing the appropriate semiring you decide what
result you get.

-}
module HyloDP.Base (
     -- ** The Types
     DPProblem(..),
     DPTypes(..),
     -- ** The Solver
     dpSolve
) where

import Data.List(foldl')
import Data.Maybe(maybeToList)
import Data.MemoTrie(HasTrie, memo)
import HyloDP.Semiring

{-|
A representation of the problem together with a description on how to
decompose it. It has three parameter types:

* @p@: the type of the instances of the problem.

* @sc@: the type of the score, the quantity that we want to maximize,
  minimize, etc.

* @d@: the type of the decisions.

-}

data DPProblem p sc d = DPProblem
     { initial :: p                     -- ^ The instance of the
                                        --   problem that has to be solved
     , isTrivial :: p -> Bool           -- ^ True if a problem is trivial
     , subproblems :: p -> [(sc, d, p)] -- ^ Returns the decomposition of a problem
     }

{- | The class 'DPTypes' is used to associate scores, decisions, and
solutions. The idea is that the same score for a decision can be
in different solutions and 'combine' associates it. For instance,
the score of a decision can be an 'Int', but the solution for a
maximization problem will be a @TMax Int@ while for a minimization
problem it will be a @TMin Int@. In other cases, the solution also
needs the decisions made, so the best solution for a maximization
problem that picks chars and has integer scores is a @BestSolution
Char (TMax Int)@. So we have:

> combine 1 'a' :: TMin Int == TMin 1
> combine 1 'a' :: TMax Int == TMax 1
> combine 1 'a' :: BestSolution Char (TMax Int) == BestSolution "a" (TMax 1)

This is the mechanism used by 'DPSolve' to choose the result.
-}

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

instance DPTypes sc d sol => DPTypes sc d (AllBestSolutions d sol) where
    combine sc d = AllBestSolutions ([[d]], combine sc d)

instance DPTypes sc (Maybe d) sol => DPTypes sc (Maybe d) (AllBestSolutions d sol) where
    combine sc d = AllBestSolutions ([maybeToList d], combine sc d)

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
data DPF sc p = Trivial | Children [(sc, p)] deriving Functor

{- |The function 'dpSolve' solves the 'initial' instance of a
'DPProblem'. The sol type is a semiring that determines what
kind of solution (the maximum, the minimum, etc.) is expected, it has
to be a 'Semiring' whose elements can be constructed from the
decisions as the scores, as determined by the 'DPTypes' constraint. The
'HasTrie' constraint ensures that memoization can be used.
-}

dpSolve :: ( HasTrie p, Semiring sol, DPTypes sc d sol)
     => DPProblem p sc d
     -> sol
dpSolve dp = hyloM solve build $ initial dp
  where build p | isTrivial dp p = Trivial
                | otherwise = Children [(combine sc d, sp) |
                                        (sc, d, sp) <- subproblems dp p]
        solve Trivial = one
        solve (Children sols) =
           foldl' (<+>) zero [ sc <.> sol | (sc, sol) <- sols ]
