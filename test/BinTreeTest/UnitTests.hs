module BinTreeTest.UnitTests where

import Test.HUnit

import BinTree.Internal
import BinTree

test_height1 = TestCase (do
  height <- return (height (BTNode 7 (BTNode 6 EmptyBT EmptyBT) (BTNode 8 EmptyBT (BTNode 9 EmptyBT (BTNode 10 EmptyBT EmptyBT)))))
  assertEqual "Height of the tree should match the pattern." True (height == 4)
  )

test_height2 = TestCase (do
  height <- return (height EmptyBT)
  assertEqual "Height of the tree should match the pattern." True (height == 0)
  )

test_height3 = TestCase (do
  height <- return (height (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  assertEqual "Height of the tree should match the pattern." True (height == 4)
  )

test_value1 = TestCase (do
  value <- return (value (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))))
  pattern <- return 4
  assertEqual "Value of the node should match the pattern." True (value == 4)
  )

bstTests = TestList [ -- height tests
                     test_height1,
                     test_height2,
                     test_height3,
                     -- value tests
                     test_value1
                     -- search tests
                     -- inorder tests
                     -- postorder tests
                     -- preorder tests
                     -- insert tests
                     -- list2BST

                    ]

-- Examples used in testing
-- BTNode 7 (BTNode 6 EmptyBT EmptyBT) (BTNode 8 EmptyBT (BTNode 9 EmptyBT (BTNode 10 EmptyBT EmptyBT)))
-- BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 EmptyBT (BTNode 6 EmptyBT (BTNode 7 EmptyBT EmptyBT)))
