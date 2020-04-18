module TextSegmentation where

import Data.Map(Map)
import qualified Data.Map.Strict as M
import Data.List(inits, tails)

import HyloDP

type Dictionary = Map String Probability

tsDPProblem :: Dictionary -> String -> DPProblem String Probability String
tsDPProblem d s = DPProblem s isTrivial subproblems
  where
    isTrivial = null
    subproblems s = [ (p, w, s')
                    | (w, s') <- tail $ zip (inits s) (tails s)
                    , Just p <- [M.lookup w d]
                    ]

ts :: Dictionary -> String -> (Maybe [String], Probability)
ts d s = (m, p)
  where BestSolution m (MaxProd p) = dpSolve $ tsDPProblem d s

tsCount :: Dictionary -> String -> Integer
tsCount d s = n
  where Count n = dpSolve $ tsDPProblem d s

tsAllSolutions :: Dictionary -> String -> [([String], Probability)]
tsAllSolutions d s = ls
  where AllSolutions ls = dpSolve $ tsDPProblem d s