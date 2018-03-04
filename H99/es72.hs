data Tree a = Node a [Tree a]

bottom_up :: Tree Char -> String
bottom_up (Node c []) = [c]
bottom_up (Node c xs) = (map bottom_up xs >>= id) ++ [c]
