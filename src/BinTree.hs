module BinTree (
    BinTree,
    height,
    left,
    right,
    value,
    inorder,
    insert,
    list2BST,
    sumBinTree
               ) where

import BinTree.Internal

insert :: (Ord a) => a -> BinTree a -> BinTree a
insert val EmptyBT = BTNode val EmptyBT EmptyBT
insert val (BTNode n lt rt) = if val < n then BTNode n (insert val lt) rt
                              else BTNode n lt (insert val rt)

list2BST :: (Ord a) => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (BTNode val lt rt) = val + sumBinTree lt + sumBinTree rt

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT _ EmptyBT = EmptyBT
mapBT f (BTNode n lt rt) = BTNode (f n) (mapBT f lt) (mapBT f rt)
