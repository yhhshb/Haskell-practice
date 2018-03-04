import Control.Monad
import Data.Char

main = forever $ do
    putStrLn "I'm young, right? (y/n)"
    a <- getChar
    getLine
    if a == 'y' || a == 'Y'
    then do
        putStrLn "Yeah, I knew it"
    else do
        putStrLn "Oh, no!"
