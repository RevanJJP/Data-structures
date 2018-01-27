module AVLTreeTest.UnitTests where

import Test.HUnit

import AVLTree
import BinTree.Internal

test_IsBalanced :: Test
test_IsBalanced = TestCase (do
  tree <- return (BTNode 3 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) EmptyBT) EmptyBT)
  assertEqual "Not balanced tree shouldn't be detected as balanced." False (balanced tree)

  tree <- return (BTNode 12 (BTNode 10 EmptyBT EmptyBT) (BTNode 20 EmptyBT EmptyBT))
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)

  tree <- return (BTNode 12 (EmptyBT) (BTNode 20 EmptyBT EmptyBT))
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)

  tree <- return (BTNode 7 (BTNode 5 EmptyBT EmptyBT) (BTNode 10 (BTNode 9 EmptyBT EmptyBT) EmptyBT))
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)

  tree <- return EmptyBT
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)

  tree <- return (BTNode 13 EmptyBT EmptyBT)
  assertEqual "Balanced tree shouldn't be detected as not balanced." True (balanced tree)

  tree <- return (BTNode 7 (BTNode 5 EmptyBT EmptyBT) (BTNode 13 (BTNode 10 (BTNode 8 EmptyBT EmptyBT) (BTNode 11 EmptyBT EmptyBT)) (BTNode 14 EmptyBT EmptyBT)))
  assertEqual "Not balanced tree shouldn't be detected as balanced." False (balanced tree)
  )

test_Rotate :: Test
test_Rotate = TestCase (do
  -- single rotation tests
  tree <- return (rotate (BTNode 3 (EmptyBT) (BTNode 4 (EmptyBT) (BTNode 5 EmptyBT EmptyBT))))
  treeRotated <- return (BTNode 4 (BTNode 3 EmptyBT EmptyBT) (BTNode 5 EmptyBT EmptyBT))
  assertEqual "Rotated tree should look like its pattern." True (tree == treeRotated)

  tree <- return (rotate (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 (EmptyBT) (BTNode 4 (EmptyBT) (BTNode 5 EmptyBT EmptyBT)))))
  treeRotated <- return (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 4 (BTNode 3 EmptyBT EmptyBT) (BTNode 5 EmptyBT EmptyBT)))
  assertEqual "Rotated tree should look like its pattern." True (tree == treeRotated)

  -- double rotation tests
  tree <- return (rotate (BTNode 5 (BTNode 3 EmptyBT (BTNode 4 EmptyBT EmptyBT)) (EmptyBT)))
  treeRotated <- return (BTNode 4 (BTNode 3 EmptyBT EmptyBT) (BTNode 5 EmptyBT EmptyBT))
  assertEqual "Rotated tree should look like its pattern." True (tree == treeRotated)
  )

-- insert tests
test_Insert :: Test
test_Insert = TestCase (do
  tree <- return (insert (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 4 (BTNode 3 EmptyBT EmptyBT) (BTNode 5 EmptyBT EmptyBT))) 6)
  treeResult <- return (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 (EmptyBT) (BTNode 6 EmptyBT EmptyBT)))
  assertEqual "Rotated tree should look like its pattern." True (tree == treeResult)

  -- insert and double rotation test
  tree <- return (insert (BTNode 1 (EmptyBT) (BTNode 3 (EmptyBT) (EmptyBT))) 2)
  treeResult <- return (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT))
  assertEqual "Rotated tree should look like its pattern." True (tree == treeResult)
  )

test_Build :: Test
test_Build = TestCase (do
  tree <- return (buildTree [1..6])
  pattern <- return (BTNode 4 (BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)) (BTNode 5 (EmptyBT) (BTNode 6 EmptyBT EmptyBT)))
  assertEqual "Built tree should look like its pattern." True (tree == pattern)
  )

avlTests = TestList [ -- balanced tests
                     test_IsBalanced,
                     -- rotate tests
                     test_Rotate,
                     -- insert tests
                     test_Insert,
                     -- buildTree tests
                     test_Build
                     ]

-- Examples used in testing
-- BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 (EmptyBT) (BTNode 4 (EmptyBT) (BTNode 5 EmptyBT EmptyBT)))
-- BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 4 (BTNode 3 EmptyBT EmptyBT) (BTNode 5 EmptyBT EmptyBT))
-- BTNode 5 (BTNode 3 EmptyBT (BTNode 4 EmptyBT EmptyBT)) (EmptyBT)
-- BTNode 4 (BTNode 3 EmptyBT EmptyBT) (BTNode 5 EmptyBT EmptyBT)
-- BTNode 1 (EmptyBT) (BTNode 3 (EmptyBT) (EmptyBT))
-- BTNode 2 (BTNode 1 EmptyBT EmptyBT) (BTNode 3 EmptyBT EmptyBT)
