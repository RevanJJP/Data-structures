module MyListTest.UnitTests where

import Test.HUnit

import MyList

test_head = TestCase (do
  head' <- return (listHead (myListFromList [1..10]))
  assertEqual "Head: sth went wrong." head' 1
  )

test_find = TestCase (do
  result <- return (listFind 0 (myListFromList [1..10]))
  assertEqual "Find: sth went wrong." False result
  )

test_find2 = TestCase (do
  result <- return (listFind 9 (myListFromList [1..10]))
  assertEqual "Find: sth went wrong." True result
  )

test_conversion = TestCase (do
  result <- return (myListToList (myListFromList [1..5]))
  pattern <- return [1..5]
  assertEqual "Conversion: sth went wrong." result pattern
  )

test_reverse = TestCase (do
  result <- return (myListToList (listReverse (myListFromList ([1..5]))))
  pattern <- return (reverse [1..5])
  assertEqual "Reverse: sth went wrong. " result pattern
  )

test_concat = TestCase (do
  result <- return (myListToList (listConcat (myListFromList [1..5]) (myListFromList [6..10])))
  pattern <- return ([6,7,8,9,10,1,2,3,4,5])
  assertEqual "Concat: sth went wrong. " result pattern
  )

test_sort1 = TestCase (do
  result <- return (sortList (Cons 1 EmptyList))
  pattern <- return (Cons 1 EmptyList)
  assertEqual "Sort: sth went wrong. " result pattern
  )

test_sort2 = TestCase (do
  result <- return (myListToList (sortList (myListFromList [6, 14, 9, 2, 3, 5, 1, 8, 7, 11, 0])))
  pattern <- return ([0,1,2,3,5,6,7,8,9,11,14])
  assertEqual "Sort: sth went wrong. " result pattern
  )

myListTests = TestList [-- head test
                        test_head,
                        -- find tests
                        test_find,
                        test_find2,
                        -- myListToList and myListFromList tests
                        test_conversion,
                        -- reverse test
                        test_reverse,
                        -- concat test
                        test_concat,
                        -- sort test
                        test_sort1,
                        test_sort2
                       ]
