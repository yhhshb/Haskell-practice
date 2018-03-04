import Data.List

sortEdges :: (Eq a, Ord a) => [(a,a,a)] -> [(a,a,a)]
sortEdges g = sortBy (\(x1,y1,z1) (x2,y2,z2) -> compare z1 z2) g

prim :: (Eq a, Ord a) => [(a,a,a)] -> [(a,a,a)]
prim g = rec_prim sorted_graph []
    where sorted_graph = sortEdges g
          rec_prim :: (Eq a, Ord a) => [(a,a,a)] -> [a] -> [(a,a,a)]
          rec_prim [] _ = []
          rec_prim ((x,y,w):sgx) vseen = 
            if xc || yc
            then (x,y,w):(rec_prim sgx vseen')
            else rec_prim sgx vseen
            where xc = not (elem x vseen)
                  yc = not (elem y vseen)
                  putVal :: Bool -> a -> [a]
                  putVal c e | c = [e] | otherwise = []
                  vseen' = (putVal xc x) ++ (putVal yc y) ++ vseen
                                             
                                              


