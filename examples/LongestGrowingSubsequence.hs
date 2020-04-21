import Data.List(tails)

import HyloDP

lgsProblem :: [Int] -> DPProblem (Int, [Int]) Int (Maybe Int)
lgsProblem xs = DPProblem (minimum xs - 1, xs) isTrivial subproblems
  where
    isTrivial = null . snd
    subproblems (m, (x:xs))
      | m >= x    = [(0, Nothing, (m, xs))]
      | otherwise = [(0, Nothing, (m, xs)),
                     (1, Just x,  (x, xs))]

main = do
  let xs = [1, 3, 2, 4, 5 , 2, 4, 8, 3, 2 ]
      problem = lgsProblem xs

  print (dpSolve problem :: TMax Int)
  print (dpSolve problem :: BestSolution Int (TMax Int))
  print (dpSolve problem :: AllBestSolutions Int (TMax Int))

