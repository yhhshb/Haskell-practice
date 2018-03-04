data Graph a = Graph [a] [(a,a)] deriving (Eq,Show)

depthfirst :: (Eq a) => Graph a -> a -> [a]
depthfirst g n = drec g [n]
    where drec :: (Eq a) => Graph a -> [a] -> [a]
          drec (Graph [] _) _ = []
          drec _ [] = []
          drec (Graph nodes edges) (top:stack)
            | [x | x<-nodes, x==top] == [] = drec (Graph nodes edges) stack
            | otherwise = top : drec (Graph (filter (\x -> x/=top) nodes) edges) ([y|(x,y)<-edges, x==top] ++ [x|(x,y)<-edges, y==top] ++ stack)
                          
connectedComponents :: (Eq a) => Graph a -> [[a]]
connectedComponents (Graph [] _) = []
connectedComponents (Graph (n:nodes) edges) = [sub] ++ connectedComponents (Graph [x | x<-nodes, not . elem x $ sub] edges)
    where sub = depthfirst (Graph (n:nodes) edges) n
