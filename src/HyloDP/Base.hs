module HyloDP.Base (
     -- ** The Dynamic Programing Problem (DPProblem) type
     DPProblem(..),
     -- ** The Dynamic Programing solver
     dpSolve
) where

import Data.List(foldl')
import Data.MemoTrie(HasTrie, memo)
import HyloDP.Semirings(Semiring(..), DPTypes(..))

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

-- | The functorial type needed by our algebra and coalgebra
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
