module Lib
    ( performer
    ) where

import System.TimeIt
import AVLTree
import BinTree.Internal
import BinTree
import MyList


performer :: IO ()
performer = do
  --timeIt $ print "Haha"
  let myList = (myListFromList [1..100000])
  --timeIt $ putStrLn (show myList ++ "\n" ++ "Wypisanie listy: ")
  let binTree = (list2BST [1..100000])
  --timeIt $ putStrLn (show binTree ++ "\n" ++ "Wypisanie drzewa BST: ")
  let avlTree = (buildTree [1..100000])
  putStrLn "haha"
