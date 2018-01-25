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
