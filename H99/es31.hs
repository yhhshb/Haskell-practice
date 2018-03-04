import Data.List

--foldr is Lazy so if accu becomes False it exits immidiately
checkPrime :: Int -> [Int] -> Bool
checkPrime c primes = foldr (\p a -> a && ((rem c p)/=0 || p == 1)) True primes

addPrime :: [Int] -> [Int]
addPrime [] = [1]
addPrime [1] = [1,2]
addPrime [1,2] = [1,2,3]
addPrime primes = filterPrime (last primes + 2) primes
    where filterPrime c primes = if checkPrime c primes then primes ++ [c]
                                 else filterPrime (c+2) primes
                                 
isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime x = checkPrime x $ init (gLessPrimes x [])
    where gLessPrimes x [] = gLessPrimes x $ addPrime []
          gLessPrimes x primes = if last primes >= x then primes
                                 else gLessPrimes x $ addPrime primes

--Da fare generatore lista infinita di numeri primi con Eratostene
