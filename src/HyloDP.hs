{-|
Module: HyloDP
Description: A simple and efficient solver for Dynamic Programming problems.
Copyright: (c) David Llorens and Juan Miguel Vilar, 2020
License: BSD-3-Clause
Stability: experimental

Practical implementation of a solver for Dynamic Programning problems.

The core ideas are:

* Using hylomorphisms as generic control flow mechanism.

* Adding memoization to avoid recomputation of intermediate results.

* Modelling the composition of solutions by means of semirings.

* Using dispatch by result type as a simple way to decide the
kind of answer desired (e.g. the best
solution, the score of that solution, or the total number of
solutions.)

A problem is described by an instance of 'DPProblem' and solved by
using 'dpSolve'. See the documentation of 'HyloDP.Base' for an
example.
-}

module HyloDP
(
  -- * Exported modules
  -- **The implementation of the solver.
  module HyloDP.Base, 
  -- **Semirings for diferent solution types.
  module HyloDP.Semirings 
)
where

import HyloDP.Base
import HyloDP.Semirings
