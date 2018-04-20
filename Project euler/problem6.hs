problem6 n = (sum [1 .. n]) ^ 2 - (sum $ map (^2) [1 .. n])

main = do
    print $ problem6 100
