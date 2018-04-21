problem9 = [a*b*c | c <- [1..1000], b <- [1..c-1], a <- [1..b-1], a^2 + b^2 == c^2, a+b+c == 1000]

main = do
    print $ head problem9
