module LongestCommonSubsequence where

import HyloDP

-- Create DPProblem

lcsDPProblem :: Eq a => [a] -> [a] -> DPProblem ([a], [a]) Int (Maybe a)
lcsDPProblem xs ys = DPProblem (xs, ys) isTrivial subproblems
  where isTrivial (xs, ys) = null xs || null ys
        subproblems (l@(x:xs), r@(y:ys))
          | x == y = [(1, Just x, (xs, ys))]
          | otherwise = [ (0, Nothing, (xs, r))
                        , (0, Nothing, (l, ys))
                        ]

-- solver

lcs :: String -> String -> String
lcs xs ys = let
       sol = dpSolve $ lcsDPProblem xs ys :: BestSolution Char (TMax Int)
    in decisions sol
