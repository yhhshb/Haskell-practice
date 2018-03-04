import Data.List

repli :: [a] -> [a]
repli = concat . map (take 2 . repeat)
