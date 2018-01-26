module MyList where

data MyList a = EmptyList | Cons a (MyList a) deriving (Show, Eq)

listHead :: MyList a -> a
listHead EmptyList = error "empty list has no head"
listHead (Cons x _) = x

listTail :: MyList a -> MyList a
listTail EmptyList = EmptyList
listTail (Cons _ xs) = xs

listFind :: (Eq a) => a -> MyList a -> Bool
listFind _ EmptyList = False
listFind n (Cons x xs) = if n == x then True
                         else listFind n xs

myListFromList :: [a] -> MyList a
myListFromList [] = EmptyList
myListFromList (x:xs) = Cons (x) (myListFromList xs)

myListToList :: MyList a -> [a]
myListToList EmptyList = []
myListToList list = (listHead list) : (myListToList (listTail list))
-- można też:
--myListToList (Cons x xs) = x : (myListToList xs)

listLength :: MyList a -> Int
listLength EmptyList = 0
listLength list = 1 + (listLength (listTail list))

lastElem :: MyList a -> a
lastElem EmptyList = error "empty list has no last element"
lastElem (Cons a EmptyList) = a
lastElem list = lastElem (listTail list)

elemAtInd :: Int -> MyList a -> a
elemAtInd _ EmptyList = error "empty list has no elements"
elemAtInd 0 list = listHead list
elemAtInd n xs = elemAtInd (n-1) (listTail xs)

listInsert :: a -> MyList a -> MyList a
listInsert x xs = Cons x xs

insertInOrder :: (Ord a) =>  a -> MyList a -> MyList a
insertInOrder x EmptyList = Cons x EmptyList
insertInOrder x list = if x < (listHead list) then Cons x list
                       else Cons (listHead list) (insertInOrder x (listTail list))

endInsert :: a -> MyList a -> MyList a
endInsert a EmptyList = Cons a EmptyList
endInsert a list = listInsert (listHead list) (endInsert a (listTail list))

listReverse :: MyList a -> MyList a
listReverse EmptyList = EmptyList
listReverse list = endInsert (listHead list) (listReverse (listTail list))

listConcat :: MyList a -> MyList a -> MyList a
listConcat list1 EmptyList = list1
listConcat EmptyList list2 = list2
listConcat (Cons a EmptyList) list2 = Cons a list2
listConcat list1 (Cons a EmptyList) = Cons a list1
listConcat list1 list2 = listInsert (listHead list2) (listConcat list1 (listTail list2))

mapList :: (a -> b) -> MyList a -> MyList b
mapList _ EmptyList = EmptyList
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

sortList :: (Ord a) => MyList a -> MyList a -- insert sort na liście
sortList EmptyList = EmptyList
sortList list = insertInOrder (listHead list) (sortList (listTail list))
