import Data.List (maximumBy)
import Data.Ord  (comparing)

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [c] = True
isPalindrome n = (head n == last n) && isPalindrome (init $ tail n)

getPalindromes3digits = filter (\(p, _) -> isPalindrome $ show p) [(x*y, (x,y)) | x <- [100 .. 999], y <- [x + 1 .. 999]]

main = do
    let (p, (x,y)) = maximumBy (comparing fst) $ getPalindromes3digits
    print (show x ++ ", " ++ show y)
