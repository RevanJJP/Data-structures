module AVLTreeTest.UnitTests where

import Test.HUnit

import AVLTree
import BinTree.Internal

test_IsBalanced1 = TestCase (do
  tree <- return (BTNode 3 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) EmptyBT) EmptyBT)
  assertEqual "Not balanced tree shouldn't be detected as balanced." False (balanced tree)
  )

test_IsBalanced2 = TestCase (do
  tree <- return (BTNode 12 (BTNode 10 EmptyBT EmptyBT) (BTNode 20 EmptyBT EmptyBT))
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)
  )

test_IsBalanced3 = TestCase (do
  tree <- return (BTNode 12 (EmptyBT) (BTNode 20 EmptyBT EmptyBT))
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)
  )

test_IsBalanced4 = TestCase (do
  tree <- return (BTNode 7 (BTNode 5 EmptyBT EmptyBT) (BTNode 10 (BTNode 9 EmptyBT EmptyBT) EmptyBT))
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)
  )

test_IsBalanced5 = TestCase (do
  tree <- return EmptyBT
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)
  )

test_IsBalanced6 = TestCase (do
  tree <- return (BTNode 13 EmptyBT EmptyBT)
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)
  )

test_IsBalanced7 = TestCase (do
  tree <- return (BTNode 7 (BTNode 5 EmptyBT EmptyBT) (BTNode 13 (BTNode 10 (BTNode 8 EmptyBT EmptyBT) (BTNode 11 EmptyBT EmptyBT)) (BTNode 14 EmptyBT EmptyBT)))
  assertEqual "Not balanced tree shouldn't be detected as balanced." False (balanced tree)
  )



avlTests = TestList [ -- balanced tests
                     test_IsBalanced1,
                     test_IsBalanced2,
                     test_IsBalanced3,
                     test_IsBalanced4,
                     test_IsBalanced5,
                     test_IsBalanced6,
                     test_IsBalanced7
                     -- rotate tests
                     -- insert tests
                     -- buildTree tests
                     ]
