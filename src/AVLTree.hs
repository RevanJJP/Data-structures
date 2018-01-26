module AVLTree where

import BinTree.Internal

balanced :: (Ord a, Num a) => BinTree a -> Bool
balanced EmptyBT = True
balanced (BTNode val lt rt) =
  if not (balanced lt) then False
  else if not (balanced rt) then False
  else if abs ((height lt) - (height rt)) > 1 then False
  else True

rotate :: (Num a, Ord a) => BinTree a -> BinTree a
rotate EmptyBT = EmptyBT
rotate (BTNode n lt rt) | not (balanced lt) = BTNode n (rotate lt) rt
                        | not (balanced rt) = BTNode n lt (rotate rt)
                        | (height lt) + 1 < (height rt) && (height (left rt)) < (height (right rt))
                          = BTNode (value rt) (BTNode n lt (left rt)) (right rt)
                        | (height rt) + 1 < (height lt) && (height (right lt)) < (height (left lt))
                          = BTNode (value lt) (left lt) (BTNode n (right lt) rt)
                        | (height lt) + 1 < (height rt) && (height (left rt)) > (height (right rt))
                          = BTNode (value (left rt)) (BTNode n lt (left (left rt))) (BTNode (value rt) (right (left rt)) (right rt))
                        | (height rt) + 1 < (height lt) && (height (right lt)) > (height (left lt))
                          = BTNode (value (right lt)) (BTNode (value lt) (left lt) (left (right lt))) (BTNode n (right (right lt)) rt)
                        | otherwise = BTNode n lt rt


insert :: (Num a, Ord a) => BinTree a -> a -> BinTree a
insert EmptyBT val = BTNode val EmptyBT EmptyBT
insert (BTNode n lt rt) val
  | n < val = rotate ((BTNode n lt (insert rt val)))
  | otherwise = rotate (BTNode n (insert lt val) rt)


buildTree :: (Num a, Ord a) => [a] -> BinTree a
buildTree [] = EmptyBT
buildTree (x:xs) = foldl insert EmptyBT (x:xs)
