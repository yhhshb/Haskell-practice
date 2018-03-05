fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
problem2 = foldl (+) 0 . filter even . takeWhile (<= 4000000) $ fibs 
