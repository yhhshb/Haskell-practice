data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

internals (Branch _ Empty Empty) = []
internals (Branch x Empty right) = [x] ++ internals right
internals (Branch x left Empty) = internals left ++ [x]
internals (Branch x left right) = internals left ++ [x] ++ internals right

atLevel :: Tree a -> Int -> [a]
atLevel (Branch x left right) 1 = [x]
atLevel (Branch x left right) l = if l > 1 then atLevel left (l-1) ++ atLevel right (l-1)
                                  else []
atLevel Empty _ = []
