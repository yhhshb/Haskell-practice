rotate xs n = drop s xs ++ take s xs
              where s = if n >= 0 then n
                        else length xs + n
