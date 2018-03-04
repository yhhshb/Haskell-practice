data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

add :: Ord a => a -> Tree a -> Tree a
add x Empty          = Branch x Empty Empty
add x (Branch y l r) = case compare x y of
                       LT -> Branch y (add x l) r
                       GT -> Branch y l (add x r)
                       EQ -> (Branch y l r)
                       
construct xs = foldl (flip add) Empty xs
