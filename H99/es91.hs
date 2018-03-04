type Position = (Int,Int)

knightMoves :: Int -> Position -> [Position]
knightMoves cbs (x,y) = filter (onBoard) [(x+a, y+b) | (a,b) <- [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]]
    where onBoard (x,y) = x > 0 && x <= cbs && y > 0 && y <= cbs
    
tour :: Int -> [[Position]]
tour cbs = loop (cbs*cbs) [[(1,1)]]
    where loop 1 = map reverse . id
          loop i = loop (i-1) . concatMap nextMoves
          nextMoves already@(x:xs) = [next:already | next <- possible]
            where possible = filter (\x -> notElem x already) $ knightMoves cbs x

