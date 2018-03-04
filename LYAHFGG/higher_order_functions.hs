multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = mod x 3829 == 0

--sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)]) 

-- Collatz chain
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (div n 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

--Lambdas!
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

example :: (Num a, Fractional a) => [a] -> [a] -> [a]
example x y = zipWith (\a b -> (a * 30 + 3) / b) x y

--map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)] 

--left fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum1 :: (Num a) => [a] -> a
sum1 = foldl (+) 0

--right fold
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

--foldl1 e foldr1 hanno accumulatore implicito uguale al primo elemento

--scan = fold con tutti i valori dell'accumulatore in lista di ritorno

negAll :: (Integral a) => [a] -> [a]
negAll xs = map (negate . abs) xs --function composition

--FUNCTION COMPOSITION = LESS PARENTHESIS
