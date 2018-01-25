module BinTree.Internal where

data BinTree a = EmptyBT | BTNode a (BinTree a) (BinTree a)
  deriving (Eq, Ord, Show, Read)

height :: (Num a, Ord a) => BinTree a -> a
height EmptyBT = 0
height (BTNode _ lt rt) = 1 + max (height lt) (height rt)

left :: BinTree a -> BinTree a
left EmptyBT = EmptyBT
left (BTNode _ lt _) = lt

right :: BinTree a -> BinTree a
right EmptyBT = EmptyBT
right (BTNode _ _ rt) = rt

value :: (Num a) => BinTree a -> a
value EmptyBT = getValue Nothing
value (BTNode val _ _) = getValue (Just val)

getValue :: Maybe a -> a
getValue Nothing = error "Nothing has no value!"
getValue (Just a) = a

search :: (Num a, Ord a) => a -> BinTree a -> Bool
search val EmptyBT = False
search val (BTNode n lt rt) = if val == n then True
                            else if val < n then search val lt
                            else search val rt

sumBT :: (Num a) => BinTree a -> a
sumBT EmptyBT = 0
sumBT (BTNode n lt rt) = n + sumBT lt + sumBT rt

inorder :: BinTree a -> [a]
inorder EmptyBT = []
inorder (BTNode val lt rt) = inorder lt ++ [val] ++ inorder rt

postorder :: BinTree a -> [a]
postorder EmptyBT = []
postorder (BTNode val lt rt) = postorder lt ++ postorder rt ++ [val]

preorder :: BinTree a -> [a]
preorder EmptyBT = []
preorder (BTNode n lt rt) = [val] ++ preorder lt ++ preorder rt
