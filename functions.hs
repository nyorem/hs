-- (maybe) useful functions

-------------
-- NUMBERS --
-------------

-- Sieve of Eratosthenes
partialCrible :: (Integral a) => [a] -> a -> [a]
partialCrible xs k = [x | x <- xs, x == k || mod x k /= 0]

f :: (Integral a) => [a] -> a -> a -> [a]
f xs k n
	| k * k < n = f (partialCrible xs k)  (k + 1) n
	| otherwise = xs

crible :: (Integral a) => a -> [a]
crible n = f [2..n] 2 n

-- Prime numbers decomposition
primeDivAux :: (Integral a) => a -> a -> a
primeDivAux n k
	| mod n k == 0 =  k
	| otherwise = primeDivAux n (k+1)

primeDiv :: (Integral a) => a -> a
primeDiv n
	| n == 1 = 1
	| otherwise = primeDivAux n 2

primeDecompo :: (Integral a) => a -> [a]
primeDecompo n
	| n == 1 = []
	| otherwise = prime : primeDecompo (n `div` prime)
	where prime = primeDiv n

isPrime :: (Integral a) => a -> Bool
isPrime n = n /= 1 && primeDiv n == n

-- pgcd
gcd' :: (Integral a) => a -> a -> a
gcd' a b
	| a == b = a
	| a < b = gcd a (b-a)
	| otherwise = gcd (a-b) b

-- fibonacci
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibo :: Int -> Integer
fibo n = fibs !! n

----------
-- SORT --
----------

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
