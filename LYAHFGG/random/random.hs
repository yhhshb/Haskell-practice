import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)
    gen2 <- getStdGen --gives the same generator, one generator per run
    putStrLn $ take 20 (randomRs ('a', 'z') gen2)
    gen' <- newStdGen --split our generator into two and updates the global random generator with one and returns the other
    putStr $ take 20 (randomRs ('a', 'z') gen')
