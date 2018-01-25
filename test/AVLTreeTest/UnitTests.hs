module AVLTreeTest.UnitTests where

import Test.HUnit

import AVLTree

test_IsBalanced1 = testCase (do
  tree <- return (BTNode 3 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) EmptyBT) EmptyBT)
  assertEqual "Not balanced" False (balanced tree)
  )
