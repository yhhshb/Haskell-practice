import Data.List

lsort' :: [[a]] -> [[a]]
lsort' = sortBy (compare length)

lfsort :: [[a]] -> [[a]]
lfsort lists = concat . lsort . groupBy (on (==) length) . lsort
