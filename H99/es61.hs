data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ left right) = countLeaves left + countLeaves right

leaves' accu Empty = accu
leaves' accu (Branch x Empty Empty) = accu ++ [x]
leaves' accu (Branch x left right) = leaves' [] left ++ leaves' [] right

leaves = leaves' []
