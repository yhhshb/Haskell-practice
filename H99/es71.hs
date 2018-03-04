data Tree a = Node a [Tree a]

ipl :: Tree a -> Int
ipl (Node e []) = 0
ipl (Node e xs) = let counters = (map ((1+) . ipl) xs)
                  in (sum counters) + sum (map (subtract 1) $ filter (>1) counters)
