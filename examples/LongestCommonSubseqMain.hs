import LongestCommonSubseq

main :: IO ()
main = do
  print $ lcs "train" "rain"
  print $ lcs "aliada" "alada"
  print $ lcs "abcd" "efg"