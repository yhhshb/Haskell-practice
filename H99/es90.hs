import Data.Maybe

--Solution from website
queens :: Int -> [[Int]]
queens n = map reverse $ queens' n
    where queens' 0       = [[]]
          queens' k       = [q:qs | qs <- queens' (k-1), q <- [1..n], isSafe q qs] --note the absence of the accumulator
          isSafe   try qs = not (try `elem` qs || sameDiag try qs) --same as checkpos
          sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs --same as checkdiag

{- My solution
queens :: Int -> [[Int]]
queens maxc = queens' maxc []
    where checkrows :: [Int] -> Int -> Bool
          checkrows rows nr = not . elem nr $ rows
          checkdiag :: [Int] -> Int -> Bool
          checkdiag rows nr = let saw = reverse [1..(length rows)]
                                  br = zipWith (+) rows saw
                                  ar = filter (\x -> x > 0) (zipWith (-) rows saw)
                              in checkrows br nr && checkrows ar nr
          checkpos :: [Int] -> Int -> Bool
          checkpos [] _ = True
          checkpos rows np = checkrows rows np && checkdiag rows np
          addpos :: [Int] -> Int -> Maybe [Int]
          addpos rows np = if checkpos rows np
                           then Just (rows ++ [np])
                           else Nothing
          queens' :: Int -> [[Int]] -> [[Int]]
          queens' maxc [] = queens' maxc (map (\x->[x]) [1..maxc])
          queens' maxc rows
              | and . map ((==maxc) . length) $ rows = rows
              | otherwise = queens' maxc (catMaybes [addpos q p | q <- rows, p<-[1..maxc]])
-}
          
