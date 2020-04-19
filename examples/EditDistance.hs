module EditDistance where

-- * A classical Dynamic Programing problem.
--
-- $EditDistance
-- Edit distance is a way of quantifying how dissimilar two strings (e.g., words)
-- are to one another by counting the minimum number of operations required to
-- transform one string into the other.

import HyloDP
import Data.Maybe(fromJust)

type EDProblem = (String, String)
type Distance = Int

data EDOperation = Ins Char | Del Char | Replace Char Char | Keep Char deriving (Eq, Show)

-- Create DPProblem

edDPProblem :: EDProblem -> DPProblem EDProblem (TMin Distance) EDOperation
edDPProblem p = DPProblem p isTrivial subproblems
  where
    isTrivial = ( == ([], []))
    subproblems (x:xs, []) = [(TMin 1, Del x, (xs, []))]
    subproblems ([], y:ys) = [(TMin 1, Ins y, ([], ys))]
    subproblems (x:xs, y:ys) = [ (s, op, (xs, ys))
                               , (TMin 1, Del x, (x:xs, ys))
                               , (TMin 1, Ins y, (xs, y:ys))]
      where
        s = TMin $ if x == y then 0 else 1
        op = if x == y then Keep x else Replace x y

-- solvers

ed :: String -> String -> Distance
ed xs ys = let (TMin sol) = dpSolve $ edDPProblem (xs, ys) in sol

edOps :: String -> String -> ([EDOperation], Distance)
edOps xs ys = let (BestSolution ops (TMin score)) = dpSolve $ edDPProblem (xs, ys) in (fromJust ops, score)