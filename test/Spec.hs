import Test.HUnit
import Test.QuickCheck

import AVLTree
import BinTree.Internal
import AVLTreeTest.UnitTests
import BinTreeTest.UnitTests
import MyListTest.UnitTests
import MyListTest.ParametricTests
import BinTreeTest.ParametricTests

--main :: IO ()
main = do
  runTestTT avlTests
  runTestTT bstTests
  runTestTT myListTests
  parametricTestsML
  parametricTestsBT
