data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch c Empty Empty) = [c]
treeToString (Branch c left right) = c : '(' : treeToString left ++ ", " ++ treeToString right ++ ")"

stringToTree :: String -> Tree Char
stringToTree "" = Empty
stringToTree [x] = Branch x Empty Empty
stringToTree str = (\("", t) -> t) . tfs $ str
    where tfs a@(x:xs) 
                | x == ',' || x == ')' = (a, Empty)
          tfs (x:y:xs)
                | y == ',' || y == ')' = (y:xs, Branch x Empty Empty)
                | y == '(' = 
                    let (',':xs', l) = tfs xs
                        (')':xs'', r) = tfs xs'
                    in  (xs'', Branch x l r)
          tfs _ = error "bad parse"
