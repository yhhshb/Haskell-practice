removeAt n xs = let m=n-1 in (xs !! m, take m xs ++ drop (m+1) xs)
