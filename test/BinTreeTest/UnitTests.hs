module BinTreeTest.UnitTests where

import Test.HUnit

import BinTree.Internal
import BinTree

test_height1 :: Test
test_height1 = TestCase (do
  height <- return (height (BTNode 7 (BTNode 6 EmptyBT EmptyBT) (BTNode 8 EmptyBT (BTNode 9 EmptyBT (BTNode 10 EmptyBT EmptyBT)))))
  assertEqual "Height of the tree should match the pattern." True (height == 4)
  )

test_height2 :: Test
test_height2 = TestCase (do
  height <- return (height EmptyBT)
  assertEqual "Height of the tree should match the pattern." True (height == 0)
  )

test_height3 :: Test
test_height3 = TestCase (do
  height <- return (height (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  assertEqual "Height of the tree should match the pattern." True (height == 4)
  )

test_value1 :: Test
test_value1 = TestCase (do
  value <- return (value (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  pattern <- return 4
  assertEqual "Value of the node should match the pattern." True (value == 4)
  )

-- test_value2 = TestCase (do
--   value <- return (value EmptyBT)
--   assertEqual "Value of the node should match the pattern." False (value == 4)
--   )

test_search1 :: Test
test_search1 = TestCase (do
  presence <- return (search 30 EmptyBT)
  assertEqual "No value should be present in an empty tree." False presence
  )

test_search2 :: Test
test_search2 = TestCase (do
  presence <- return (search 30 (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  assertEqual "No value should be present in an empty tree." False presence
  )

test_search3 :: Test
test_search3 = TestCase (do
  presence <- return (search 5 (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  assertEqual "No value should be present in an empty tree." True presence
  )

test_inorder :: Test
test_inorder = TestCase (do
  result <- return (inorder (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  pattern <- return [1..7]
  assertEqual "Inorder: something went wrong." result pattern
  )

test_postorder :: Test
test_postorder = TestCase (do
  result <- return (postorder (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  pattern <- return [1, 3, 2, 7, 6, 5, 4]
  assertEqual "Postorder: something went wrong." result pattern
  )

test_preorder :: Test
test_preorder = TestCase (do
  result <- return (preorder (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  pattern <- return [4, 2, 1, 3, 5, 6, 7]
  assertEqual "Preorder: something went wrong." result pattern
  )

test_list2BST :: Test
test_list2BST = TestCase (do
  result <- return (list2BST [4, 2, 1, 3, 5, 6, 7])
  pattern <- return (BTNode 7 (BTNode 6 (BTNode 5 (BTNode 3 (BTNode 1 EmptyBT (BTNode 2 EmptyBT EmptyBT)) (BTNode 4 EmptyBT EmptyBT)) EmptyBT) EmptyBT) EmptyBT)
  assertEqual "List2BST: something went wrong." True (result == pattern)
  )

test_map :: Test
test_map = TestCase (do
  result <- return (mapBinTree (^2) (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  pattern <- return (BTNode 16 (BTNode 4 (BTNode 1 EmptyBT EmptyBT) (BTNode 9 EmptyBT EmptyBT)) (BTNode 25 EmptyBT (BTNode 36 EmptyBT (BTNode 49 EmptyBT EmptyBT))))
  assertEqual "mapBinTree: something went wrong." True (result == pattern)
  )


bstTests = TestList [ -- height tests
                     test_height1,
                     test_height2,
                     test_height3,
                     -- value tests
                     test_value1,
                     -- test_value2,
                     -- search tests
                     test_search1,
                     test_search2,
                     test_search3,
                     -- inorder tests
                     test_inorder,
                     -- postorder tests
                     test_postorder,
                     -- preorder tests
                     test_preorder,
                     -- list2BST tests
                     test_list2BST,
                     -- map tests
                     test_map
                    ]

-- Examples used in testing
-- BTNode 7 (BTNode 6 EmptyBT EmptyBT) (BTNode 8 EmptyBT (BTNode 9 EmptyBT (BTNode 10 EmptyBT EmptyBT)))
-- BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))
-- BTNode 7 (BTNode 6 (BTNode 5 (BTNode 3 (BTNode 1 EmptyBT (BTNode 2 EmptyBT EmptyBT)) (BTNode 4 EmptyBT EmptyBT)) EmptyBT) EmptyBT) EmptyBT
