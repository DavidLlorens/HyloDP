-- For HasTrie:
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RandomWalk where

import Data.MemoTrie
import GHC.Generics(Generic)

import HyloDP
import Probability

type Position = Int
type Step = Int

data RWProblem = RW { from :: Position, to :: Position, remaining :: Step } deriving Show

-- HasTrie instance for RWProblem

deriving instance Generic RWProblem

instance HasTrie RWProblem where
  newtype (RWProblem :->: b) = RWTrie { unRWTrie :: Reg RWProblem :->: b }
  trie = trieGeneric RWTrie
  untrie = untrieGeneric unRWTrie
  enumerate = enumerateGeneric unRWTrie

-- Create DPProblem

rwDPProblem :: RWProblem -> DPProblem RWProblem Probability ()
rwDPProblem p = DPProblem p isTrivial subproblems
  where isTrivial p = from p >= to p
        subproblems (RW _ _ 0) = []
        subproblems (RW p f s) = [ (0.5, (), RW (p + 1) f (s - 1))
                                 , (0.5, (), RW (p - 1) f (s - 1))
                                 ]

-- solver

rw :: Position -> Position -> Step -> Probability
rw from to rem = dpSolve problem
  where rwDPP = (RW from to rem)
        problem = rwDPProblem rwDPP
