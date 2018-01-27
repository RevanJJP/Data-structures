{-# LANGUAGE FlexibleInstances #-}

module MyListTest.ParametricTests where

import MyList
import Test.QuickCheck

instance Arbitrary (MyList Int) where
  arbitrary = do
    values <- arbitrary :: Gen [Int]
    return $ myListFromList values


prop_firstEl :: MyList Int -> Bool
prop_firstEl list = (listLength list == 0) || (listHead list == head (myListToList list))

prop_tail :: MyList Int -> Bool
prop_tail list = (listLength list == 0) || (myListToList (listTail list) == tail (myListToList list))

prop_length :: MyList Int -> Bool
prop_length list = (listLength list == length (myListToList list))

prop_insert_sort :: Int -> MyList Int -> Bool
prop_insert_sort x list = (sortList $ listInsert x list) == (insertInOrder x $ sortList list)

-- inserts values from a non-sorted list1 to a sorted list2
help_list_concat :: MyList Int -> MyList Int -> MyList Int
help_list_concat EmptyList list2 = list2
help_list_concat list1 list2 = (help_list_concat (listTail list1) (insertInOrder (listHead list1) list2))

prop_list_concat :: MyList Int -> MyList Int -> Bool
prop_list_concat list1 list2 = (sortList (listConcat list1 list2) == help_list_concat (list1) (sortList list2))

prop_list_eq :: MyList Int -> Bool
prop_list_eq list = list == list

parametricTestsML = do
  quickCheck prop_firstEl
  quickCheck prop_tail
  quickCheck prop_length
  quickCheck prop_insert_sort
  quickCheck prop_list_eq
