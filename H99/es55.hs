data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

cbaTree :: Int -> [Tree Char]
cbaTree 0 = [Empty]
cbaTree n = let (q,r) = (quotRem (n-1) 2)
    in [Branch 'x' left right | i <- [q..q+r],
                                left <- cbaTree i,
                                right <- cbaTree (n-1-i)]
