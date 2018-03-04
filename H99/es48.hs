import Data.Typeable (typeOf)
import Control.Monad (replicateM)

{-NON COMPLETO!
table :: f -> IO()
table f = do
          tf <- f True
          ff <- f False
          if (show $ typeOf tf) == "Bool" then putStrLn $ show tf
          else do
            putStr "True "
            table tf
          if (show $ typeOf ff) == "Bool" then putStrLn $ show ff
          else do
            putStr "False "
            table ff
-}
     
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ " => " ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "
