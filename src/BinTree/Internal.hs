{-|
  Module name: Bintree.Internal
  Description: our own Haskell implementation of a Binary Search Trees, provides basic (common) features of 'BinTree' and 'AVLTree'.
-}

module BinTree.Internal where

-- | Definition of 'BinTree' data type.
data BinTree a = EmptyBT -- ^ Empty node
  | BTNode a (BinTree a) (BinTree a) -- ^ Node with a left and a right son
  deriving (Ord, Show, Read)

-- | Returns height of the tree.
height :: (Num b, Ord b) => BinTree a -> b
height EmptyBT = 0
height (BTNode _ lt rt) = 1 + max (height lt) (height rt)

-- | Returns value of node's left son.
left :: BinTree a -> BinTree a
left EmptyBT = EmptyBT
left (BTNode _ lt _) = lt

-- | Returns value of node's right son.
right :: BinTree a -> BinTree a
right EmptyBT = EmptyBT
right (BTNode _ _ rt) = rt

-- | Returns node's value (assuming that empty BinTree has no value!)
value :: (Num a) => BinTree a -> a
value EmptyBT = getValue Nothing
value (BTNode val _ _) = getValue (Just val)

-- | Gets node's value (attempt to get value of empty BinTree calls an error).
getValue :: Maybe a -> a
getValue Nothing = error "Nothing has no value!"
getValue (Just a) = a

-- | Checks if a given value exists in a BinTree.
search :: (Num a, Ord a) => a -> BinTree a -> Bool
search val EmptyBT = False
search val (BTNode n lt rt) = if val == n then True
                            else if val < n then search val lt
                            else search val rt

-- | Performs an inorder conversion to simple list, for example:
--  inorder BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))
--  => [1, 2, 3, 4, 5, 6, 7]
inorder :: BinTree a -> [a]
inorder EmptyBT = []
inorder (BTNode val lt rt) = inorder lt ++ [val] ++ inorder rt

-- | Performs a postorder conversion to simple list, for example:
-- postorder BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))
-- => [1, 3, 2, 7, 6, 5, 4]
postorder :: BinTree a -> [a]
postorder EmptyBT = []
postorder (BTNode val lt rt) = postorder lt ++ postorder rt ++ [val]

-- | Performs a preorder conversion to simple list, for example:
--  preorder BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))
--  => [4, 2, 1, 3, 5, 6, 7]
preorder :: BinTree a -> [a]
preorder EmptyBT = []
preorder (BTNode val lt rt) = [val] ++ preorder lt ++ preorder rt

-- | Checks if two BinTrees are equal, which means that they contain the same values.
ifEqBinTrees :: Eq a => BinTree a -> BinTree a -> Bool
ifEqBinTrees EmptyBT (BTNode n lt rt) = False
ifEqBinTrees (BTNode n lt rt) EmptyBT = False
ifEqBinTrees EmptyBT EmptyBT = True
ifEqBinTrees (BTNode n1 lt1 rt1) (BTNode n2 lt2 rt2) = n1 == n2 && (ifEqBinTrees lt1 lt2) && (ifEqBinTrees rt1 rt2)

-- | Adding BinTree to class Eq.
instance Eq a => Eq (BinTree a) where
	(==) t1 t2 = ifEqBinTrees t1 t2
