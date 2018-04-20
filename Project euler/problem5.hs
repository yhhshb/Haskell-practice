import Data.List

primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

-- tree-merging Eratosthenes sieve, primesTME of Q.31, 
--  adjusted to produce primes in a given range (inclusive)
primesR a b | b < a || b < 2 = []
            | otherwise      = takeWhile (<= b) $ primesFrom a
 
primesFrom a0 = (if a0 <= 2 then [2] else []) ++ 
                (gaps a $ mults $ span (< z) $ tail primesTME)
  where
    a = snap (max 3 a0) 3 2
    z = ceiling $ sqrt $ fromIntegral a + 1       -- p<z => p*p<=a
    snap v origin step = if r==0 then v else v+(step-r)
        where r = rem (v-origin) step   -- NB: origin <= v ; else use MOD
 
    mults (h,p':t) =                              -- p'>=z => p'*p'>a
      join union ( [[x,x+s..] | p <- h,           -- heads unordered  
                            let s=2*p; x=snap a (p*p) s]
                   ++ [[p'*p',p'*p'+2*p'..]] )
      `union'` join union' [[p*p,p*p+2*p..] | p <- t]
 
    join  f (xs:t)    = f xs (join f (pairs f t))
    join  f []        = []
    pairs f (xs:ys:t) = f xs ys : pairs f t
    pairs f t         = t
    union' (x:xs) ys  = x : union xs ys           -- `union` of Q.31
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs
                                 
getPrimesLessThan n = primesR 2 n

primeFactors :: Int -> [Int]
primeFactors n = case factors of
    [] -> [n]
    _  -> factors ++ primeFactors (div n (head factors))
    where factors = take 1 $ filter (\x -> (mod n x) == 0) [2 .. n-1]
    
findMaxMult :: Int -> [[Int]] -> Int
findMaxMult q xs = maximum $ map (length . filter (q==)) xs

smallestMult n = [findMaxMult p (map primeFactors [2 .. n]) | p <- getPrimesLessThan n]

problem5 n = product $ zipWith (^) (getPrimesLessThan n) (smallestMult n)

main = do
    print $ problem5 20
