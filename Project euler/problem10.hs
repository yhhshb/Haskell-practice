import Primes

problem10 = sum $ primesRange 2 (2 * 10^6)

main = do
    print problem10
