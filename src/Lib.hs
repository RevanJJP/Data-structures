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

  putStrLn "Rozmiar danych: 1000"
  let binTree1 = (list2BST [1..1000])
  let avlTree1 = (buildTree [1..1000])

  -- umowne
  putStrLn "Tworzenie BST: "
  timeIt $ print (search 1000 binTree1)
  putStrLn "Tworzenie AVL: "
  timeIt $ print (search 512 avlTree1)

  let !binTree = (list2BST [1..1000])
  let !avlTree = (buildTree [1..1000])
  putStrLn "\n"
  -- umowne
  putStrLn "Wyszukiwanie w drzewie BST: "
  timeIt $ print (search 0 binTree)
  putStrLn "Wyszukiwanie w drzewie AVL: "
  timeIt $ print (search 0 avlTree)

-------------------------------------------------------------------
  putStrLn "Rozmiar danych: 10000"
  let !binTree = (list2BST [1..10000])
  let !avlTree = (buildTree [1..10000])
  putStrLn "\n"
  -- umowne
  putStrLn "Wyszukiwanie w drzewie BST: "
  timeIt $ print (search 0 binTree)
  putStrLn "Wyszukiwanie w drzewie AVL: "
  timeIt $ print (search 0 avlTree)

  putStrLn "Mozna zauwazyc, ze choc czas zbudowania drzewa AVL jest znacznie wiekszy od czasu zbudowania zwyklego drzewa BST,"
  putStrLn "to z kolei znaczaca zaleta drzewa AVL jest szybsze wyszukiwanie niz w przypadku drzewa BST."
