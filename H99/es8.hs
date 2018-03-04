import Data.List --per compress'

merge :: (Eq a) => [a] -> a -> [a]
merge [] x = [x]
merge acc x = if last acc == x then acc else acc ++ [x]

compress :: (Eq a) => [a] -> [a]
compress xs = foldl merge [] xs

compress' = map head . group
