import Data.List

data Tree a = Node a [Tree a] deriving (Eq, Show)

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [], Node 'e' []]]

display :: (Show a) => Tree a -> String
display (Node a []) = [(show a) !! 1]
display (Node a xs) = "(" ++ [(show a) !! 1] ++ " " ++ intercalate " " (map display xs) ++ ")"
