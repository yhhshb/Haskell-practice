primeFactors :: Int -> [Int]
primeFactors n = case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (div n (head factors))
    where factors = take 1 $ filter (\x -> (mod n x) == 0) [2 .. n-1]
    
main = do
    print (maximum $ primeFactors 600851475143)
