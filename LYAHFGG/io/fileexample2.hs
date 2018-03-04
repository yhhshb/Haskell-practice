import System.IO

main = do
    contents <- readFile "avril"
    writeFile "AVRIL" (map toUpper contents)
    appendFile "Appender.txt" "appended"
