import Data.List (sortBy)
import Data.Set (toList, fromList)

data Graph a = Graph [a] [(a,a)]
type Color = Int

degree :: Eq a => Graph a -> a -> Int
degree (Graph nodes edges) n = length . filter (\(x,y) -> x==n || y==n) $ edges

sortByDegree :: Eq a => Graph a -> [a]
sortByDegree (Graph nodes edges) = fst . unzip . sortBy (\(n1,d1) (n2,d2)-> compare d2 d1) . zip nodes . map (degree (Graph nodes edges)) $ nodes

kcolor' :: (Eq a, Ord a) => [a] -> [(a,a)] -> Color -> [(a,Color)]
kcolor' [] _ _ = []
kcolor' (n:sn) edges c = accu ++ kcolor' sn' edges' (c+1)
    where kcolRecur :: (Eq a, Ord a) => [a] -> [(a,a)] -> Color -> [(a,Color)]
          kcolRecur [] _ _ = []
          kcolRecur (n:sn) edges c = let distant_edges = filter (\(x,y) -> x/=n && y/=n) edges
                                         close_edges = filter (\(x,y) -> x==n || x ==n) edges
                                         distant_sn = filter (\x -> not . elem x $ (fst . unzip $ close_edges) ++ (snd . unzip $ close_edges)) sn
                                     in [(n,c)] ++ kcolRecur distant_sn distant_edges c
          distant_edges = filter (\(x,y) -> x/=n && y/=n) edges
          close_edges = filter (\(x,y) -> x==n || x ==n) edges
          distant_sn = filter (\x -> not . elem x $ (fst . unzip $ close_edges) ++ (snd . unzip $ close_edges)) sn
          accu = [(n,c)] ++ kcolRecur distant_sn distant_edges c
          sn' = (filter (\x -> not . elem x $ fst . unzip $ accu) sn)
          edges' = filter (\(x,y) -> elem x sn' && elem y sn') distant_edges
          
kcolor :: (Eq a,Ord a) => Graph a -> [(a,Color)]
kcolor (Graph nodes edges) = sortBy (\(n1,c1) (n2,c2) -> compare n1 n2) $ kcolor' (sortByDegree (Graph nodes edges)) edges 1

bipartite :: (Eq a,Ord a) => Graph a -> Bool
bipartite g = (length . toList . fromList . snd . unzip $ kcolor g) == 2
          
          
          
