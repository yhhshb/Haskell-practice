data ListItem a = Single a | Multiple Int a deriving (Show)

encode' :: Eq a => [a] -> [(Int,a)]
encode' = foldr helper []
    where
      helper x [] = [(1,x)]
      helper x ((a,b):ys)
        | x == b    = (1+a,b):ys
        | otherwise = (1,x):(a,b):ys
 
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
    where
        encodeHelper (1,x) = Single x
        encodeHelper (n,x) = Multiple n x
