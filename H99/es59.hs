data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

hbalTree :: a -> Int -> [Tree a]
hbalTree e 0 = []
hbalTree e 1 = [Branch e Empty Empty]
hbalTree e h = [Branch e left right | (hl, hr) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)],
                                      left <- hbalTree e hl,
                                      right <- hbalTree e hr]
