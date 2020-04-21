import LongestCommonSubsequence
import HyloDP

main :: IO ()
main = do
  print (dpSolve $ lcsDPProblem "train" "raising" :: BestSolution Char (TMax Int))
  print (dpSolve $ lcsDPProblem "train" "raising" :: TMax Int)
  print $ lcs "train" "rain"
  print $ lcs "aliada" "alada"
  print $ lcs "abcd" "efg"
