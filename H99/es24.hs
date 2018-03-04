import System.Random
import Control.Monad (replicateM)

diff_elem :: [Int] -> IO (Int, [Int])
diff_elem l = do
    index <- getStdRandom . randomR $ (0, (length l)-1)
    return (l !! index, (take index l) ++ (drop (index + 1) l))

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
    l <- return [1..m]
    accu <- return []
    diff_helper accu l n where
        diff_helper :: [Int] -> [Int] -> Int -> IO [Int]
        diff_helper accu l 0 = return accu
        diff_helper accu l n = do
            (e, xs) <- diff_elem l
            diff_helper (e:accu) xs (n-1)
