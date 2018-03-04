respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines
    where isPalindrome xs = xs == reverse xs
    
main = interact respondPalindromes
