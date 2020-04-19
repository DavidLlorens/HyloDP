{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HyloDP.Semirings (
  -- * Classes
  Semiring(..),
  DPTypes(..),
  Opt(..),
  -- * Types
  -- ** Probability semiring
  Probability(..),
  -- ** Min tropical semiring
  TMin(..),
  -- ** Max tropical semiring
  TMax(..),
  -- ** Max product semiring
  MaxProd(..),
  -- ** Count semiring
  Count(..),
  -- ** Best solution semiring
  BestSolution(..),
  -- ** All solutions semiring
  AllSolutions(..),
  -- * Other functions
  decisions
) where

import Data.Maybe(maybeToList, fromJust)

-- ----------------------
-- Typeclass definitions
-- ----------------------

-- | Wikipedia: [Semiring](https://en.wikipedia.org/wiki/Semiring).
class Semiring s where
    infixl 6 <+>
    (<+>) :: s -> s -> s
    infixl 7 <.>
    (<.>) :: s -> s -> s
    zero  :: s
    one   :: s

-- | This typeclass is used in optimization semirings.
class Opt t where
  optimum :: t -> t -> t

-- | The typeclass used for combining a score and a decision to form a solution
class DPTypes sc d sol where
    combine :: sc -> d -> sol

-- | Trivial instance if the solution is just the score
instance DPTypes sc d sc where
    combine = const

-- --------------------
-- Semiring definitions
-- --------------------

newtype Probability = Probability Double deriving(Show, Eq, Ord, Fractional, Num)

-- | Trivial semiring for probabilities (used in 'RandomWalk' and 'TextSegmentation' examples)
instance Semiring Probability where
  (<+>) = (+)
  (<.>) = (*)
  zero = 0
  one = 1

instance Bounded Probability where
  maxBound = 1
  minBound = 0

-- ***********************************************************************************

-- | Trivial semiring for integers (used in 'Fibonacci' example)
instance Semiring Integer where
  (<+>) = (+)
  (<.>) = (*)
  zero = 0
  one = 1

-- ***********************************************************************************

newtype TMin v = TMin v deriving (Eq, Ord, Show)
newtype TMax v = TMax v deriving (Eq, Ord, Show)

-- | Min tropical semiring. Minimizes the sum of the scores (used in the 'EditDistance' example)
instance (Num v, Ord v, Bounded v) => Semiring (TMin v) where
  t1 <+> t2 = min t1 t2
  t1@(TMin v1) <.> t2@(TMin v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = TMin (v1 + v2)
  zero = TMin maxBound
  one = TMin 0

-- | Max tropical semiring. Maximizes the sum of the scores  (used in 'Knapsack' and 'LongestCommonSubseq' examples)
instance (Num v, Ord v, Bounded v) => Semiring (TMax v) where
  t1 <+> t2 = max t1 t2
  t1@(TMax v1) <.> t2@(TMax v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = TMax (v1 + v2)
  zero = TMax minBound
  one = TMax 0

instance Ord v => Opt (TMin v) where
  optimum = min

instance Ord v => Opt (TMax v) where
  optimum = max

instance DPTypes sc d (TMin sc) where
    combine = const . TMin

instance DPTypes sc d (TMax sc) where
    combine = const . TMax

-- ***********************************************************************************

newtype MaxProd v = MaxProd v deriving (Eq, Ord, Show)

-- | Maximize the product of the scores (used in the 'TextSegmentation' example)
instance (Num v, Ord v, Bounded v) => Semiring (MaxProd v) where
  t1 <+> t2 = max t1 t2
  t1@(MaxProd v1) <.> t2@(MaxProd v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = MaxProd (v1 * v2)
  zero = MaxProd minBound
  one = MaxProd 1

instance Ord v => Opt (MaxProd v) where
  optimum = max

instance DPTypes sc d (MaxProd sc) where
  combine = const . MaxProd

-- ***********************************************************************************

newtype Count = Count Integer deriving Show

-- | Counts the number of solutions.
instance Semiring Count where
  Count n <+> Count n' = Count $ n + n'
  Count n <.> Count n' = Count $ n * n'
  zero = Count 0
  one = Count 1

instance DPTypes sc d Count where
  combine = const . const $ Count 1

-- ***********************************************************************************

data BestSolution d sc = BestSolution (Maybe [d]) sc deriving (Eq, Show)

-- | If we have an 'Opt' semiring (like 'TMax', 'TMin' or 'MaxProd') we can
-- build this semiring to get both the decisions of the best solution and its score.
instance (Semiring sc, Opt sc, Eq sc) => Semiring (BestSolution d sc) where
    sol1@(BestSolution _ sc1) <+> sol2@(BestSolution _ sc2)
       | optimum sc1 sc2 == sc1 = sol1
       | otherwise = sol2
    BestSolution ds1 sc1 <.> BestSolution ds2 sc2 =
       BestSolution ((++) <$> ds1 <*> ds2) (sc1 <.> sc2)
    zero = BestSolution Nothing zero
    one = BestSolution (Just []) one

instance DPTypes sc d sol => DPTypes sc d (BestSolution d sol) where
    combine sc d = BestSolution (Just [d]) (combine sc d)

instance DPTypes sc (Maybe d) sol => DPTypes sc (Maybe d) (BestSolution d sol) where
    combine sc d = BestSolution (Just $ maybeToList d) (combine sc d)

-- | Extracts the decisions from a BestSolution.
decisions :: BestSolution d sc -> [d]
decisions (BestSolution s _) = fromJust s

-- ***********************************************************************************

newtype AllSolutions d s = AllSolutions [([d], s)] deriving Show

-- | Obtains all the solutions as a list of pairs ([decision], score).
instance Semiring sol => Semiring (AllSolutions d sol) where
  AllSolutions sols <+> AllSolutions sols' = AllSolutions (sols ++ sols')
  AllSolutions sols <.> AllSolutions sols' =
    AllSolutions [ (sol ++ sol', sc <.> sc')
                 | (sol, sc) <- sols, (sol', sc') <- sols']
  zero = AllSolutions []
  one = AllSolutions [([], one)]

instance DPTypes sc d sol => DPTypes sc d (AllSolutions d sol) where
  combine sc d = AllSolutions [([d], combine sc d)]

instance DPTypes sc (Maybe d) sol => DPTypes sc (Maybe d) (AllSolutions d sol) where
  combine sc d = AllSolutions [(maybeToList d, combine sc d)]

-- | A tuple with two semirings.
instance (Semiring s1, Semiring s2) => Semiring (s1, s2) where
  (s1, s2) <+> (s1', s2') = (s1 <+> s1', s2 <+> s2')
  (s1, s2) <.> (s1', s2') = (s1 <.> s1', s2 <.> s2')
  zero = (zero, zero)
  one = (one, one)

instance (DPTypes sc d sol, DPTypes sc d sol') => DPTypes sc d (sol, sol') where
  combine sc d = (combine sc d, combine sc d)
