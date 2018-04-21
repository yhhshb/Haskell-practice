module Primes where
import Data.List (union)

primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
    join  ((x:xs):t)        = x : union xs (join (pairs t))
    pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
    gaps k xs@(x:t) | k==x  = gaps (k+2) t 
                    | True  = k : gaps (k+2) xs

-- tree-merging Eratosthenes sieve, primesTME of Q.31, 
--  adjusted to produce primes in a given range (inclusive)
primesRange a b | b < a || b < 2 = []
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
