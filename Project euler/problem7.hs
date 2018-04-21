import Primes

main = do
    print (last $ take 10001 $ primesFrom 2)
