import Test.HUnit

import AVLTree
import BinTree.Internal
import AVLTreeTest.UnitTests
import BinTreeTest.UnitTests

--main :: IO ()
main = do
  runTestTT avlTests
  runTestTT bstTests
