{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module: HyloDP.Semiring
Description: Declaration of the Semiring class and various instances
Copyright: (c) David Llorens and Juan Miguel Vilar, 2020
License: BSD-3-Clause
Stability: experimental

This module declares the 'Semiring' class and various instances.
-}
module HyloDP.Semiring (
  -- *Type Classes
  Semiring(..),
  Opt(..),
  -- *Semiring Helpers
  -- ** Min tropical semiring
  TMin(..),
  -- ** Max tropical semiring
  TMax(..),
  -- ** Min product semiring
  MinProd(..),
  -- ** Max product semiring
  MaxProd(..),
  -- ** Count semiring
  Count(..),
  -- ** Best solution semiring
  BestSolution(..),
  -- ** All solutions semiring
  AllSolutions(..),
  -- ** All best solutions semiring
  AllBestSolutions(..),
  -- * Other functions
  decisions
) where

import Data.Maybe(fromJust)

-- ----------------------
-- Typeclass definitions
-- ----------------------

{- | A 'Semiring' is a type with two operations '<+>' and '<.>' and two
distinguished elements, 'zero' and 'one', which satisfy the following
axioms:

* Conmutativity:

> a <+> b == b <+> a
> a <.> b == b <.> a

* Associativity:

> a <+> (b <+> c) == (a <+> b) <+> c
> a <.> (b <.> c) == (a <.> b) <.> c

* Identity:

> a <+> zero = zero <+> a == a
> a <.> one = one <.> a == a

* Distributive property:

> a <.> (b<+>c) == (a<.>b) <+> (a<.>c)
> (a<+>b) <.>c == (a<.>c) <+> (b<.>c)

* Anhiliation of multiplication by zero:

> a <.> zero = zero <.> a = zero
-}
class Semiring s where
    infixl 6 <+>
    (<+>) :: s -> s -> s
    infixl 7 <.>
    (<.>) :: s -> s -> s
    -- |Neutral element for '<+>'.
    zero  :: s
    -- |Neutral element for '<.>'.
    one   :: s

-- | This typeclass is used in optimization semirings. It is expected
-- that @optimum a b@ returns either @a@ or @b@.
class Opt t where
  optimum :: t -> t -> t

-- --------------------
-- Semiring definitions
-- --------------------

-- Number instances

instance Semiring Int  where
  (<+>) = (+)
  (<.>) = (*)
  zero = 0
  one = 1

instance Semiring Integer  where
  (<+>) = (+)
  (<.>) = (*)
  zero = 0
  one = 1

instance Semiring Float  where
  (<+>) = (+)
  (<.>) = (*)
  zero = 0
  one = 1

instance Semiring Double  where
  (<+>) = (+)
  (<.>) = (*)
  zero = 0
  one = 1

-- |The tropical min semiring, a semiring that uses 'min' as '<+>' and
-- sum as '<.>'. It is used in problems that ask for minimizing a sum of
-- values.
newtype TMin v = TMin v deriving (Eq, Ord, Show)

-- |The tropical max semiring, a semiring that uses 'max' as '<+>' and
-- sum as '<.>'. It is used in problems that ask for maximizing a sum of
-- values.
newtype TMax v = TMax v deriving (Eq, Ord, Show)

instance (Num v, Ord v, Bounded v) => Semiring (TMin v) where
  t1 <+> t2 = min t1 t2
  t1@(TMin v1) <.> t2@(TMin v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = TMin (v1 + v2)
  zero = TMin maxBound
  one = TMin 0

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

-- | The 'MinProd' semiring is the analogous to the 'TMin' semiring
-- for minimizing products.
newtype MinProd v = MinProd v deriving (Eq, Ord, Show)

instance (Num v, Ord v, Bounded v) => Semiring (MinProd v) where
  t1 <+> t2 = max t1 t2
  t1@(MinProd v1) <.> t2@(MinProd v2)
    | t1 == zero = zero
    | t2 == zero = zero
    | otherwise = MinProd (v1 * v2)
  zero = MinProd maxBound
  one = MinProd 1

instance Ord v => Opt (MinProd v) where
  optimum = min

-- | The 'MaxProd' semiring is the analogous to the 'TMax' semiring
-- for maximizing products.
newtype MaxProd v = MaxProd v deriving (Eq, Ord, Show)

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

-- |The 'Count' semiring is used for counting the number of different
-- solutions.
newtype Count = Count Integer deriving Show

instance Semiring Count where
  Count n <+> Count n' = Count $ n + n'
  Count n <.> Count n' = Count $ n * n'
  zero = Count 0
  one = Count 1

-- |The `BestSolution` semiring is used for recovering the best sequence
-- of decisions together with its score. The score must be an instance of
-- 'Opt' to be able to decide which is the best of two scores.
data BestSolution d sc = BestSolution (Maybe [d]) sc deriving (Eq, Show)

instance (Semiring sc, Opt sc, Eq sc) => Semiring (BestSolution d sc) where
    sol1@(BestSolution _ sc1) <+> sol2@(BestSolution _ sc2)
       | optimum sc1 sc2 == sc1 = sol1
       | otherwise = sol2
    BestSolution ds1 sc1 <.> BestSolution ds2 sc2 =
       BestSolution ((++) <$> ds1 <*> ds2) (sc1 <.> sc2)
    zero = BestSolution Nothing zero
    one = BestSolution (Just []) one

-- |Auxiliary function to recover the sequence of decisions from a `BestSolution`
decisions :: BestSolution d sc -> [d]
decisions (BestSolution s _) = fromJust s

-- |With the 'AllSolutions' semiring it is possible to recover all possible
-- solutions to a problem, regardless of their scores.
newtype AllSolutions d sc = AllSolutions [([d], sc)] deriving Show

instance Semiring sc => Semiring (AllSolutions d sc) where
  AllSolutions sols1 <+> AllSolutions sols2 = AllSolutions (sols1 ++ sols2)
  AllSolutions sols1 <.> AllSolutions sols2 =
    AllSolutions [ (ds1 ++ ds2, sc1 <.> sc2) 
                 | (ds1, sc1) <- sols1, (ds2, sc2) <- sols2]
  zero = AllSolutions []
  one = AllSolutions [([], one)]

-- |With the 'AllBestSolutions' semiring it is possible to recover all the
-- solutions to a problem that reach the optimum score.
newtype AllBestSolutions d s = AllBestSolutions ([[d]], s) deriving Show

instance (Semiring sc, Opt sc, Eq sc) => Semiring (AllBestSolutions d sc) where
  a1@(AllBestSolutions (sols1, sc1)) <+> a2@(AllBestSolutions (sols2, sc2))
    | sc1 == sc2 = AllBestSolutions (sols1 ++ sols2, sc1)
    | optimum sc1 sc2 == sc1 = a1
    | otherwise = a2
  AllBestSolutions (sols1, sc1) <.> AllBestSolutions (sols2, sc2) =
    AllBestSolutions ((++) <$> sols1 <*> sols2, sc1 <.> sc2)
  zero = AllBestSolutions ([], zero)
  one = AllBestSolutions ([[]], one)

instance (Semiring s1, Semiring s2) => Semiring (s1, s2) where
  (s1, s2) <+> (s1', s2') = (s1 <+> s1', s2 <+> s2')
  (s1, s2) <.> (s1', s2') = (s1 <.> s1', s2 <.> s2')
  zero = (zero, zero)
  one = (one, one)
