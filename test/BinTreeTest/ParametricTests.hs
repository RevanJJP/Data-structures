{-# LANGUAGE FlexibleInstances #-}

module BinTreeTest.ParametricTests where

import BinTree.Internal
import BinTree
import Test.QuickCheck
import MyList

instance Arbitrary (BinTree Int) where
  arbitrary = do
    values <- arbitrary :: Gen [Int]
    return $ list2BST values

prop_order :: BinTree Int -> Bool
prop_order tree = (myListFromList (inorder tree)) == (sortList $ myListFromList (postorder tree))

prop_sum :: Int -> BinTree Int -> Bool
prop_sum x tree = (sumBinTree (insert x tree)) == (sumBinTree tree) + x

prop_eq :: BinTree Int -> Bool
prop_eq tree = tree == tree

prop_search :: Int -> BinTree Int -> Bool
prop_search x tree = (search x $ insert x tree) == True

parametricTestsBT = do
  quickCheck prop_order
  quickCheck prop_sum
  quickCheck prop_eq
  quickCheck prop_search
