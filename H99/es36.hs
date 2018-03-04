import Data.List

factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime

primeFactorsPairs = (map encode) . group . factor
    where encode xs = (head xs, length xs)
