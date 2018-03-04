paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths source sink g
    | source == sink = [[sink]]
    | otherwise = [source:path | edge <- g, (fst edge) == source, path <- (paths (snd edge) sink [e | e <- g, e /= edge])]

cycle' :: Eq a => a -> [(a,a)] -> [[a]]
cycle' n g = (if (elem (n,n) g) 
            then [[n]]
            else []) ++ (map ([n]++) . concat . map rec $ stps)
            where stps = filter (/=n) . map snd . filter ((==n).fst) $ g
                  rec s = paths s n g
