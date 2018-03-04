split :: [a] -> Int -> [[a]]
split xs n = if length xs > n then helper [[], xs] n
             else [xs, []]
    where helper :: [[a]] -> Int -> [[a]]
          helper xs 0 = xs
          helper (xs1:xs2:xs) n = helper [xs1 ++ [xs2 !! 0], tail' xs2] (n-1)
              where tail' [] = []
                    tail' (x:xs) = xs
