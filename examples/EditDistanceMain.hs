import EditDistance

main :: IO ()
main = do
  let x = "train"
      y = "basin"
  print(ed x y)     -- just the distance
  print(edOps x y)  -- the seq. of edition operations and the distance
