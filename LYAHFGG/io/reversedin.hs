main = do
    line <- getLine
    if null line
        then do
            return () --encapsulates () into an IO action, DOES NOT INTERRUPT the program
            main
        else do
            putStrLn $ reverseWords line
            main
            
reverseWords :: String -> String
reverseWords = unwords . map reverse . words
