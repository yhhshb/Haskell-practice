import Data.List
import Primes
                                 
getPrimesLessThan n = primesRange 2 n

primeFactors :: Integer -> [Integer]
primeFactors n = case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (div n (head factors))
    where factors = take 1 $ filter (\x -> (mod n x) == 0) [2 .. n-1]
    
findMaxMult :: Integer -> [[Integer]] -> Int
findMaxMult q xs = maximum $ map (length . filter (q==)) xs

smallestMult n = [findMaxMult p (map primeFactors [2 .. n]) | p <- getPrimesLessThan n]

problem5 n = product $ zipWith (^) (getPrimesLessThan n) (smallestMult n)

main = do
    print $ problem5 20
