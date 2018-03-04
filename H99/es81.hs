paths :: Eq a => a -> a -> [(a,a)] -> [[a]]
paths source sink g
    | source == sink = [[sink]]
    | otherwise = [source:path | edge <- g, (fst edge) == source, path <- (paths (snd edge) sink [e | e <- g, e /= edge])]
