import Data.List

data ListItem a = Single a | Multiple Int a deriving (Show)

decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : decodeModified xs
decodeModified ((Multiple n x):xs) = (take n . repeat) x ++ decodeModified xs
