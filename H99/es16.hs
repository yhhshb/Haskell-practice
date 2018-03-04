dropEvery :: [a] -> Int -> [a]
dropEvery xs n = if (length . take n) xs == n then ((init . take n) xs) ++ (dropEvery (drop n xs) n)
                 else xs
