import Knapsack

main :: IO ()
main = do
  let capacity = 100
      items = [Item 20 40, Item 30 20, Item 40 50, Item 50 60, Item 20 10]
  print $ ks capacity items
  print $ ksItems capacity items
  print $ ksCount capacity items
  print $ ksAllSols capacity items
