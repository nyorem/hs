-- higher order function : function that returns a function or that takes a function as a parameter
-- WARNING: every function in haskell can have only one parameter. We 'curried' multiple parameters functions
-- space means function application
-- ex: max is a function that takes an 'a' as a parameter and returns a function that takes an 'a' as parameter and returns an 'a'
-- if we call a function with less parameters, we obtain a partial function
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- infix functions can also be partially applied -> surround with parentheses. It is called sections
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

uppercase :: Char -> Bool
uppercase = (`elem` ['A' .. 'Z'])

-- WARNING: if we want to use '-', we must use subtract function

-- WARNING: when we put things in ghci, it calculates the result first and then print it to the screen using the show function
-- functions are not part of the Show typeclass

-- functions that take functions as parameters
-- WARNING: need parentheses to distinguish functions from parameters
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- example: applyTwice (+3) 10

-- zipWith f x y : join w-two lists by applying a function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

-- flip f : function with two arguments that are flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
	where g x y = f y x
-- example: flip zip [1..] "hello"

-- map : apply a function on a list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x): map' f xs

-- use cases : it is more readadble than list comprehensions
-- produce a list of square
sq = map (^2) [1..]
-- list of first element of list of tuples
f = map fst [(1,2), (2, 3), (3, 4)]
-- replication
r = map (replicate 3) [1..10]

-- filter : keep things in a list that verify a predicate
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x:filter' f xs
	| otherwise = filter' f xs

-- usage : like list comprehensions with predicates
-- keep odd numbers in a list
o = filter odd [1..100]
-- keep numbers > a in a list
n = filter (>10) [1..20]
-- keep uppercase characters
c = filter (`elem` ['A'..'Z']) "salut A B C"
-- quicksort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort less) ++ [x] ++ (qsort greater)
	where greater = filter (> x) xs
	      less = filter (<= x) xs

-- exercise : find largest number under 100,000 that's divisible by 3829
res = head (filter p [100000, 99999 ..])
	where p x = (x `mod` 3829) == 0

-- takeWhile : take elements in a list while a predicate is satisfied
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
	| f x = x: takeWhile' f xs
	| otherwise = []

word = takeWhile (/= ' ') "long sentence"

-- exercise : find the sum of all odd squares that are smaller than 10,000
-- demonstration of Haskell laziness
res2 = sum (takeWhile (<= 10000) (filter odd (map (^2) [1..])))

-- exercise : Syracuse conjecture (ends with 1)
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| odd n = n:chain (3 * n + 1)
	| otherwise = n:chain (n `div` 2)
-- for all starting numbers between 1 and 100, how many chains have a length greater than 15?
res3 = length (filter p (map chain [1..100]))
	where p xs = length xs >= 15

-- list of functions
lf = map (*) [0..] -- lf = [(0*), (1*), ..]

-- lambdas : anonymous functions
-- (\ param -> body)
res4 = length (filter (\xs -> length xs >= 15) (map chain [1..100]))

-- lambdas are expressions (more precisely, it is an expression that returns a function)
s = zipWith (\a b -> a + 2 * b) [1, 2, 3] [4, 8, 10]

-- pattern match in a lambda
t = map (\(a, b) -> a + b) [(1, 2), (3, 4), (5, 6)]

-- flip bis
flipBis :: (a -> b -> c) -> (b -> a -> c)
flipBis f = \x y -> f y x

-- folds : take a binary function, an accumulator and a list to fold
-- HOW IT WORKS: binary(accumulator, element) -> new accumulator

-- sum bis with left fold (== right fold by commutation)
sumBis :: (Num a) => [a] -> a
sumBis = foldl (+) 0

-- WARNING: foo a = bar b a <-> foo = bar b (currying)

-- elem bis
elemBis :: (Eq a ) => a -> [a] -> Bool
elemBis x = foldl (\acc y -> acc || x == y) False

-- right fold : binary function (x, acc) ==> used when building things (like lists)
-- WARNING: can be used in infinite lists /= left folds
-- map bis using right fold (left fold reverses the list)
mapBis :: (a -> b) -> [a] -> [b]
mapBis f = foldr (\x acc -> f x : acc) []

-- revserse bis
reverseBis :: [a] -> [a]
reverseBis = foldl (\acc x -> x : acc) []

-- WARNING: folds are used when we traversed a list once element by element and return something based on that
