module BinTree.Internal where

data BinTree a = EmptyBT | BTNode a (BinTree a) (BinTree a)
  deriving (Ord, Show, Read)

height :: (Num b, Ord b) => BinTree a -> b
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

inorder :: BinTree a -> [a]
inorder EmptyBT = []
inorder (BTNode val lt rt) = inorder lt ++ [val] ++ inorder rt

postorder :: BinTree a -> [a]
postorder EmptyBT = []
postorder (BTNode val lt rt) = postorder lt ++ postorder rt ++ [val]

preorder :: BinTree a -> [a]
preorder EmptyBT = []
preorder (BTNode val lt rt) = [val] ++ preorder lt ++ preorder rt

ifEqBinTrees :: Eq a => BinTree a -> BinTree a -> Bool
ifEqBinTrees EmptyBT (BTNode n lt rt) = False
ifEqBinTrees (BTNode n lt rt) EmptyBT = False
ifEqBinTrees EmptyBT EmptyBT = True
ifEqBinTrees (BTNode n1 lt1 rt1) (BTNode n2 lt2 rt2) = n1 == n2 && (ifEqBinTrees lt1 lt2) && (ifEqBinTrees rt1 rt2)

instance Eq a => Eq (BinTree a) where
	(==) t1 t2 = ifEqBinTrees t1 t2
