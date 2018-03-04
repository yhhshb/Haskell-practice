data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

areSpecular :: Tree a -> Tree a -> Bool
areSpecular Empty Empty = True
areSpecular (Branch lv lleft lright) (Branch rv rleft rright) = (areSpecular lleft rright) && (areSpecular lright rleft)
areSpecular _ _ = False

symmetric :: Tree a -> Bool
symmetric (Branch v left right) = areSpecular left right
symmetric Empty = True
