import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

filled :: Tree a -> [[Bool]]
filled Empty = repeat [False]
filled (Branch _ l r) = [True] : zipWith (++) (filled l) (filled r)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generate_tree 1
    where generate_tree x
        | x > n     = Empty
        | otherwise = Branch 'x' (generate_tree (2*x)) (generate_tree (2*x+1))
        
isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree t = and $ last_proper : zipWith (==) lengths powers
    where levels      = takeWhile or $ filled t
          -- The upper levels of the tree should be filled.
          -- Every level has twice the number of nodes as the one above it,
          -- so [1,2,4,8,16,...]
          lengths     = map (length . filter id) $ init levels --equal to: length . filter (==True)
          powers      = iterate (2*) 1
          -- The last level must contain a number of filled nodes, all at the beginning
          -- and (maybe) some empty nodes after those filled.
          last_filled = map head $ group $ last levels -- group the last filled vector in True and False groups
          last_proper = head last_filled && (length last_filled) < 3 -- The group list should contain only two elements, the first one (the head of the first grouped list) should be True.
          
