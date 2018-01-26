module BinTree where

import BinTree.Internal

insert :: (Ord a) => a -> BinTree a -> BinTree a
insert val EmptyBT = BTNode val EmptyBT EmptyBT
insert val (BTNode n lt rt) = if val < n then BTNode n (insert val lt) rt
                              else BTNode n lt (insert val rt)

list2BST :: (Eq a, Ord a) => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (BTNode val lt rt) = val + sumBinTree lt + sumBinTree rt

mapBinTree :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBinTree _ EmptyBT = EmptyBT
mapBinTree f (BTNode n lt rt) = BTNode (f n) (mapBinTree f lt) (mapBinTree f rt)
