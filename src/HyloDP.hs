{-|
Module: HyloDP
Description: Simple and effective resolution of dynamic programming problems.
Copyright: (c) David Llorens and Juan Miguel Vilar, 2020
License: BSD-3-Clause
Stability: experimental

Practical implementation of a category theory approach for DP problem solving. The core ideas are:

    * The use of hylomorphims with memoization for the DP core process: problem decomposition and solution composition.

    * The use of semirings for the composition of the subproblem solutions.

Futhermore, it will be trivial to change
the composition for obtaining different answers, say the best
solution, the score of that solution, or the total number of
solutions.
-}

module HyloDP
(
  -- * DP solver using hylomorphims with memoization.
  module HyloDP.Base,
  -- * Semirings for diferent solution types.
  module HyloDP.Semirings
)
where

import HyloDP.Base
import HyloDP.Semirings