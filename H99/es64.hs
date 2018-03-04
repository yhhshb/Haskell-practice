data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

{-
getNodeX :: Tree (a,(Int,Int)) -> Int
getNodeX Empty = 0
getNodeX (Branch (e,(x,y)) left right) = x

update :: Tree (a,(Int,Int)) -> Int -> Tree (a,(Int,Int))
update Empty _ = Empty
update (Branch (e,(x,y)) left right) offset = Branch (e,((x+offset),y)) (update left offset) (update right offset)

count :: Tree a -> Int
count Empty = 0
count (Branch e left right) = 1 + count left + count right

layout :: Tree a -> Int -> Tree (a,(Int,Int))
layout Empty y = Empty
layout (Branch e left right) y = (Branch (e,(x,y)) left' right')
    where left'  = layout left (y+1)
          x      = count left + 1
          right' = update (layout right (y+1)) x
-}

type Pos = (Int,Int)

layout :: Tree a -> Tree (a, Pos)
layout t = fst (layoutAux 1 1 t)
  where layoutAux x y Empty = (Empty, x)
        layoutAux x y (Branch e l r) = (Branch (e, (x',y)) l' r', x'')
          where (l', x')  = layoutAux x (y+1) l
                (r', x'') = layoutAux (x'+1) (y+1) r
           
