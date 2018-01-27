{-|
  Module name: MyList
  Description: our own implementation of a list in Haskell
-}

module MyList where

-- | Definition of our data type.
data MyList a = EmptyList | Cons a (MyList a) deriving (Show)

-- | Returns head of our list.
listHead :: MyList a -> a
listHead EmptyList = error "empty list has no head"
listHead (Cons x _) = x

-- | Returns tail of the list (all the elements except for its head).
listTail :: MyList a -> MyList a
listTail EmptyList = EmptyList
listTail (Cons _ xs) = xs

-- | Checks if a given value exists in the list.
listFind :: (Eq a) => a -> MyList a -> Bool
listFind _ EmptyList = False
listFind n (Cons x xs) = if n == x then True
                         else listFind n xs

-- | Builds and returns list of type 'MyList' on the basis of a regular Haskell Data.List.
myListFromList :: [a] -> MyList a
myListFromList [] = EmptyList
myListFromList (x:xs) = Cons (x) (myListFromList xs)

-- | Builds a Data.List list on the basis of 'MyList' list
myListToList :: MyList a -> [a]
myListToList EmptyList = []
myListToList list = (listHead list) : (myListToList (listTail list))
-- można też:
-- myListToList (Cons x xs) = x : (myListToList xs)

-- | Returns length of a given list.
listLength :: MyList a -> Int
listLength EmptyList = 0
listLength list = 1 + (listLength (listTail list))

-- | Returns last elements of a given list.
lastElem :: MyList a -> a
lastElem EmptyList = error "empty list has no last element"
lastElem (Cons a EmptyList) = a
lastElem list = lastElem (listTail list)

-- | Looks for an element of a given index in the list and returns it if it exists.
elemAtInd :: Int -> MyList a -> a
elemAtInd _ EmptyList = error "empty list has no elements"
elemAtInd 0 list = listHead list
elemAtInd n xs = elemAtInd (n-1) (listTail xs)

-- | Inserts a single element into a given list.
listInsert :: a -> MyList a -> MyList a
listInsert x xs = Cons x xs

-- | Inserts a single element into a given list, assuming that the given list is sorted.
insertInOrder :: (Ord a) =>  a -> MyList a -> MyList a
insertInOrder x EmptyList = Cons x EmptyList
insertInOrder x list = if x < (listHead list) then Cons x list
                       else Cons (listHead list) (insertInOrder x (listTail list))

-- | Inserts a single element onto the end of a given list (except for an EmptyList, where it creates a new node at the beginning).
endInsert :: a -> MyList a -> MyList a
endInsert a EmptyList = Cons a EmptyList
endInsert a list = listInsert (listHead list) (endInsert a (listTail list))

-- | Reverses whole 'MyList' list.
-- Example (simplification):
-- listReverse [1, 2, 3, 4, 5] => [5, 4, 3, 2, 1]
listReverse :: MyList a -> MyList a
listReverse EmptyList = EmptyList
listReverse list = endInsert (listHead list) (listReverse (listTail list))

-- | Connects two 'MyList' lists together.
listConcat :: MyList a -> MyList a -> MyList a
listConcat list1 EmptyList = list1
listConcat EmptyList list2 = list2
listConcat (Cons a EmptyList) list2 = Cons a list2
listConcat list1 (Cons a EmptyList) = Cons a list1
listConcat list1 list2 = listInsert (listHead list2) (listConcat list1 (listTail list2))

-- | Performs 'MyList''s mapping, for example (simplification):
-- mapList (^2) [1, 2, 3, 4, 5] => [1, 4, 9, 16, 25]
mapList :: (a -> b) -> MyList a -> MyList b
mapList _ EmptyList = EmptyList
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

-- | Performs insertion sort on a 'MyList'.
sortList :: (Ord a) => MyList a -> MyList a -- insert sort na liście
sortList EmptyList = EmptyList
sortList list = insertInOrder (listHead list) (sortList (listTail list))

-- | Checks if two lists are equal.
ifEqLists :: Eq a => MyList a -> MyList a -> Bool
ifEqLists EmptyList EmptyList = True
ifEqLists list1 list2 = if (listLength list1) /= (listLength list2) then False
  else (listHead list1 == listHead list2) && (ifEqLists (listTail list1) (listTail list2))

-- | Adding MyList to class Eq.
instance Eq a => Eq (MyList a) where
  (==) list1 list2 = ifEqLists list1 list2
