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
-- WARNING: does not work well with infinite lists -> takeWhile
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

-- foldl1 and foldr1 variants : same like foldl and foldr except the starting value is the first element of the list
-- WARNING: throw a runtime error when run on an empty list
sum'' :: (Num a) => [a] -> a
sum'' = foldl1 (+)

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) [] -- <=> foldl (flip (:)) []

product'' :: (Num a) => [a] -> a
product'' = foldl1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head'' :: [a] -> a
head'' = foldr1 (\x _ -> x)

last'' :: [a] -> a
last'' = foldl1 (\_ x -> x)

-- scanl and scanr : like foldl and foldr except they print every intermediate value of the accumulator
-- scanl1 and scanr1 also exist
-- examples
a = scanl1 (+) [1 .. 10]
b = scanr1 (+) [1 .. 10]
aa = scanl1 (\acc x -> if x > acc then x else acc) [1, 3, 9, 0, 8, 11, 9]

-- WARNING: used to monitor the progression of a function that can be implemented with a fold

-- exercise : How many elements does it take for the sum of the roots of all natural numbers to exceed 1000 ?
res5 = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function application with $
-- $ has the lowest precedence and right associative /= ' ' which has the high precedence
-- $ = opening parentheses to the end of the expression
-- used to not right extra parentheses
dollar = sum $ map sqrt [1..30]
dollar2 = sqrt $ 3 + 4 + 9
-- f (g (z x)) <-> f $ g $ z x
dollar3 = sum $ filter (>30) $ map (^2) [1..10]
-- ($) is a function!
funList = map ($ 4) [(+3), (+4), sqrt]

-- function composition with .
-- negate : opposite of the number
comp1 = map (negate . abs) [-10..10]
-- '.' is right associative : (f . g . h) x = f (g (h x))
-- WARNING: if we have multiple parameters => last parameter of the innermost function after a $ and then put dots between others functions (without their last parameter)
-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8]))) turns into :
comp2 = replicate 100 . product . map (*3) . zipWith max [1, 2, 3, 4, 5] $ [4, 5, 6, 7, 8]

-- point free style : currying functions
-- sum xs = foldl (+) 0 xs ==> sum = foldl (+) 0
fn x = ceiling (negate (tan (cos (max 50 x))))
fnFree = ceiling . negate . tan . cos . max 50

-- point-free : more readable and concise if the function is not very complex
oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSumFree :: Integer
oddSquareSumFree = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

-- too much composition : use let bindings to give name to intermediate results
oddSquareSumFreeBis :: Integer
oddSquareSumFreeBis = let oddSquares = filter odd $ map (^2) [1..];
			  belowLimit = takeWhile (<10000) oddSquares
		      in sum belowLimit
