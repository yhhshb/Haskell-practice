import Data.List

encode xs = map (\x -> (length x, head x)) (group xs)
