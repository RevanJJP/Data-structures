{-| Module name: BinTree
  | Description: our own Haskell implementation of a Binary Search Tree, uses features of 'BinTree.Internal'.
-}

module BinTree (
  BinTree,
  insert,
  height,
  list2BST,
  sumBinTree,
  mapBinTree
    ) where

import BinTree.Internal

-- | Inserts single value into a BinTree.
insert :: (Ord a) => a -> BinTree a -> BinTree a
insert val EmptyBT = BTNode val EmptyBT EmptyBT
insert val (BTNode n lt rt) = if val < n then BTNode n (insert val lt) rt
                              else BTNode n lt (insert val rt)

-- | Builds a BinTree on the basis of a list.
list2BST :: (Eq a, Ord a) => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = insert x (list2BST xs)

-- | Sums all values in a BinTree.
sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (BTNode val lt rt) = val + sumBinTree lt + sumBinTree rt

-- | Performs BinTree mapping, for example:
-- mapBinTree (^2) BTNode 7 (BTNode 6 EmptyBT EmptyBT) (BTNode 8 EmptyBT (BTNode 9 EmptyBT (BTNode 10 EmptyBT EmptyBT)))
-- => BTNode 49 (BTNode 36 EmptyBT EmptyBT) (BTNode 64 EmptyBT (BTNode 81 EmptyBT (BTNode 100 EmptyBT EmptyBT)))
mapBinTree :: (a -> b) -> BinTree a -> BinTree b -- map function for a binary tree
mapBinTree _ EmptyBT = EmptyBT
mapBinTree f (BTNode n lt rt) = BTNode (f n) (mapBinTree f lt) (mapBinTree f rt)
