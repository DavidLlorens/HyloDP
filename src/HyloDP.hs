-- | HyloDP is a library for easily solving dynamic programing problems.
-- This approach requires some knowledge about the problem:
--
--    1. How the problem decompose into smaller instances.
--
--    2. How the solutions to the smaller instances compose to
--       produce the solution of the original instance.
--
-- Futhermore, it will be trivial to change
-- the composition for obtaining different answers, say the best
-- solution, the score of that solution, or the total number of
-- solutions. Two factors made this possible: the use of the concept of
-- semiring to abstract the operations performed with the solutions and
-- the use of return type polymorphism in Haskell.

module HyloDP
(
  module HyloDP.Base,
  module HyloDP.Semirings
)
where

import HyloDP.Base
import HyloDP.Semirings