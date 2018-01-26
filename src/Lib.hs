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

  let myList1 = (myListFromList [1..1000])
  let binTree1 = (list2BST [1..1000])
  let avlTree1 = (buildTree [1..1000])

  -- umowne
  putStrLn "Tworzenie listy: "
  timeIt $ print (listFind 1 myList1)
  putStrLn "Tworzenie BST: "
  timeIt $ print (search 1000 binTree1)
  putStrLn "Tworzenie AVL: "
  timeIt $ print (search 512 avlTree1)

  let !myList = (myListFromList $ reverse [1..1000])
  --timeIt $ putStrLn (show myList ++ "\n" ++ "Wypisanie listy: ")
  let !binTree = (list2BST [1..1000])
  --timeIt $ putStrLn (show binTree ++ "\n" ++ "Wypisanie drzewa BST: ")
  let !avlTree = (buildTree [1..1000])
  putStrLn "\n"
  -- umowne
  putStrLn "Wyszukiwanie w liscie: "
  timeIt $ print (listFind 500 myList)
  putStrLn "Wyszukiwanie w drzewie BST: "
  timeIt $ print (search 1 binTree)
  putStrLn "Wyszukiwanie w drzewie AVL: "
  timeIt $ print (search 1 avlTree)

  putStrLn "Mozna zauwazyc, ze choc czas zbudowania drzewa AVL jest znacznie wiekszy od czasu zbudowania zwyklego drzewa BST,"
  putStrLn "to z kolei znaczaca zaleta drzewa AVL jest o wiele szybsze wyszukiwanie niz w przypadku drzewa BST."
