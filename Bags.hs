module Bags where
 --An Item is a tuple of two elements, the first is a number,string,char etc and the second element is the number
 --of times the first element occurs in the multiset/Bag
type Item a = (a,Int)
--A Bag is a list of Items
type Bag a = [Item a]

--Inserts individual elements of the list into an empty bag one by one.
listToBag :: Eq a => [a] -> Bag a
listToBag [] = []
listToBag (h:t) = bagInsert h (listToBag t)    
---------------------------------------------------------------------------------------------------------------------------------------------------------------
--Returns true if every item in the first Bag is present in the second Bag and vice-versa
bagEqual :: Eq a => Bag a -> Bag a -> Bool 
bagEqual [] [] = True
bagEqual [] _ = False
bagEqual _ [] = False
bagEqual bag1 bag2 = allMembers bag1 bag2 && allMembers bag2 bag1 

--Returns true if every element in list1 is also present in list2.
--Recursion stops when an element in list1 is absent in list2 or when all elements of list1 are in list2.
allMembers :: Eq a => [a] -> [a] -> Bool
allMembers [] [] = True
allMembers [] _ = True
allMembers _ [] = False
allMembers (h:t) list2 
           | elem h list2 = allMembers t list2
           | otherwise = False
-------------------------------------------------------------------------------------------------------------------------------------------------
--If the element being inserted already exists in the Bag, increments the count of that item by 1 and returns resulting Bag. 
--Otherwise, adds the new element with a count of 1 and returns the resulting Bag.
bagInsert :: Eq a => a -> Bag a -> Bag a
bagInsert a [] = [(a,1)]  
bagInsert a ((h,n):t)
           |a == h = ((h,n + 1) : t)
           |otherwise = ((h,n) : bagInsert a t) 
---------------------------------------------------------------------------------------------------------------------------------------------------
--Individually adds items from the first Bag into the second one, and returns the resulting Bag 
bagSum :: Eq a => Bag a -> Bag a -> Bag a   
bagSum [] bag2 = bag2
bagSum bag1 [] = bag1
bagSum (h:t) bag2 = bagSum t (addItem h bag2)

--Takes a single Item and a Bag and adds the Item to the Bag, returning the resulting Bag.
--If the Item being inserted already exists in the Bag, adds both their counts and returns resulting Bag.
--Otherwise, adds the Item to end of the Bag and returns the resulting Bag.
addItem :: Eq a => Item a -> Bag a -> Bag a
addItem a [] = [a]
addItem (h1,num1) ((h2,num2):t) 
           | h1 == h2 = ((h1,num1+num2) : t) 
           | otherwise = ((h2,num2):addItem (h1,num1) t)
-------------------------------------------------------------------------------------------------------------------------------------------------------
--Returns the bag containing Items common in both Bag1 and Bag2, by individually comparing items in Bag1 with those in Bag2.
bagIntersection :: Eq a => Bag a -> Bag a -> Bag a
bagIntersection [] bag2 = []
bagIntersection bag1 [] = []
bagIntersection (h:t) bag2 = bagSum (elementIntersection h bag2) (bagIntersection t bag2) 

--Takes an Item and a Bag and returns a Bag containing only that Item if that Item is present the Bag given.
--otherwise, returns an empty Bag
elementIntersection :: Eq a => Item a -> Bag a -> Bag a
elementIntersection a [] = []
elementIntersection (h1,num1) ((h2,num2):t) 
           | h1 == h2 = [(h1, min num1 num2)] --final count equals the lesser of the counts of the two items
           | otherwise =  elementIntersection (h1,num1) t
          