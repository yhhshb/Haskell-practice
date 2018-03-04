import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen = 
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)
    
nCoins :: Int -> StdGen -> [Bool]
nCoins n gen = take n $ randoms gen
    
main = do
    print $ threeCoins (mkStdGen 100)
    print $ nCoins 10 (mkStdGen 100)
