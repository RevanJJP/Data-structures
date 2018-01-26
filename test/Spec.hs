import Test.HUnit

import AVLTree
import BinTree.Internal
import AVLTreeTest.UnitTests
import BinTreeTest.UnitTests
import MyListTest.UnitTests

--main :: IO ()
main = do
  runTestTT avlTests
  runTestTT bstTests
  runTestTT myListTests
