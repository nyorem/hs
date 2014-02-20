-----------------------
-- SORTING ALORITHMS --
-----------------------

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
		 where lesser = [y | y <- xs, y <= x]
		       greater = [y | y <- xs, y > x]

-- merge sort
-- cut a list into two lists
segmente :: [a] -> ([a], [a])
segmente [] = ([], [])
segmente [x] = ([x], [])
segmente (x:y:xs) = (x:g, y:d)
		  where (g, d) = segmente xs

-- merge two sorted lists
merge :: (Ord a) => [a] -> [a] -> [a]
merge l  [] = l
merge [] l = l
merge allX@(x:xs) allY@(y:ys)
	| x <= y = x:merge xs allY
	| otherwise = y:merge ys allX

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort l = merge (mergesort g) (mergesort d)
	    where (g, d) = segmente l

-- insertion sort
-- insert an element into a sorted list
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
	| x <= y = x:(y:ys)
	| otherwise = y:(insert x ys)

insertionsort :: (Ord a) => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = insert x (insertionsort xs)
