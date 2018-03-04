import Data.List (nub) --import only nub from Data.List
--import Data.List hiding (nub) --import everything except nub
import qualified Data.Map as M

--nub rimuove gli elementi duplicati in una lista
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

--see documentation for list of useful functions in each module


