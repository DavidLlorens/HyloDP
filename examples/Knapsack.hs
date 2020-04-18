-- For HasTrie:
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Knapsack where

import Data.MemoTrie
import GHC.Generics(Generic)
import Data.Maybe(fromJust)

import HyloDP

type Capacity = Int
type Value = Int
type Weight = Int
data Item = Item { value :: Value, weight :: Weight } deriving (Eq, Show)
data KSProblem = KSP { capacity :: Capacity, items :: [Item] }

-- HasTrie instances for Item and KSProblem

deriving instance Generic Item

instance HasTrie Item where
  newtype (Item :->: b) = ItemTrie { unItemTrie :: Reg Item :->: b }
  trie = trieGeneric ItemTrie
  untrie = untrieGeneric unItemTrie
  enumerate = enumerateGeneric unItemTrie

deriving instance Generic KSProblem

instance HasTrie KSProblem where
  newtype (KSProblem :->: b) = KSTrie { unKSTrie :: Reg KSProblem :->: b }
  trie = trieGeneric KSTrie
  untrie = untrieGeneric unKSTrie
  enumerate = enumerateGeneric unKSTrie

-- Create DPProblem

ksDPProblem :: KSProblem -> DPProblem KSProblem Value (Maybe Item)
ksDPProblem p = DPProblem p isTrivial subproblems
  where
    isTrivial = null . items
    subproblems (KSP c (i:is))
      | value i <= c = [(0, Nothing, KSP c is) , (value i, Just i, (KSP (c-weight i) is))]
      | otherwise = [(0, Nothing, KSP c is)]


-- solvers

ks :: Capacity -> [Item] -> Value
ks capacity items = let (TMax v) = dpSolve problem  in v
  where
    kpDPP = KSP capacity items
    problem = ksDPProblem kpDPP

ksItems :: Capacity -> [Item] -> ([Item], Value)
ksItems capacity items = let (BestSolution sol (TMax v)) = dpSolve problem  in (fromJust sol, v)
  where
    kpDPP = KSP capacity items
    problem = ksDPProblem kpDPP

ksCount :: Capacity -> [Item] -> Integer
ksCount capacity items = let Count n = dpSolve problem in n
  where
    kpDPP = KSP capacity items
    problem = ksDPProblem kpDPP

ksAllSols :: Capacity -> [Item] -> [([Item], TMax Value)]
ksAllSols capacity items = let AllSolutions sols = dpSolve problem in sols
  where
    kpDPP = KSP capacity items
    problem = ksDPProblem kpDPP
