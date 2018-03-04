combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] = const [[]] --return [[]] for each list of parameters [a]
group (g:gs) = concatMap helper $ combination g xs
    where helper (as, bs) = map (as:) (group gs bs)
