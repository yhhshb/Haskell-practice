gray :: Integral a => a -> [String]
gray 0 = [""]
gray 1 = ["0", "1"]
gray n = let xs = gray (n-1) in (map ("0" ++) xs) ++ (map ("1" ++) . reverse $ xs)
