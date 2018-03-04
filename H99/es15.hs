repli' :: [a] -> Int -> [a]
repli' xs n = concat . map (take n . repeat) $ xs
