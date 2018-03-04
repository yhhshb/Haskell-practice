pack' :: (Eq a) => [[a]] -> a -> [[a]]
pack' [[]] x = [[x]]
pack' acc x = if (head . last $ acc) == x
              then (init acc) ++ [last acc ++ [x]]
              else acc ++ [[x]]


pack :: (Eq a) => [a] -> [[a]]
pack xs = foldl pack' [[]] xs
