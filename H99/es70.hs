data Tree a = Node a [Tree a] deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node a []) = 1
nnodes (Node a xs) = 1 + (sum $ map nnodes xs)
