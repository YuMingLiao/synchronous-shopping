module PairHeap where

data Heap a = Empty | Heap a [(Heap a)]
    deriving Show

findMin :: Heap a -> a
findMin (Heap h _) = h

minView :: Ord a => Heap a -> Maybe (a, Heap a)
minView (Heap x hs) = Just (x, mergePairs hs)
minView Empty = Nothing

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1@(Heap x hs1) h2@(Heap y hs2)
    | x < y     = Heap x (h2:hs1)
    | otherwise = Heap y (h1:hs2)

mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs []           = Empty
mergePairs [h]          = h
mergePairs (h1:h2:hs)   = merge (merge h1 h2) (mergePairs hs)

insert :: Ord a => a -> Heap a -> Heap a
insert x = merge (Heap x [])

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap x hs) = mergePairs hs
