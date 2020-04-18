import RandomWalk

main :: IO ()
main = print $ rw from to remaining
  where from = 0
        to = 2
        remaining = 6