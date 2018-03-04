totient 1 = 1
totient a = length $ filter (\b -> gcd a b == 1) [1..a-1]
