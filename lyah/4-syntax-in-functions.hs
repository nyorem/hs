-- pattern matching
-- soecify a list of patterns, and when you invoke a function, compiler will try to find from top to bottom, a corresponding pattern and then execute it
lucky :: (Integral a) => a -> String
lucky 7 = "Got seven!"
lucky x = "Bad luck!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

-- order is important : specific ones must be on top
-- WARNING: patterns must be exhaustive
fact :: (Integral a) => a -> a
fact 0 = 1
fact n = n * fact (n-1)

-- pattern matching with tuples
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- '_' means everything ==> useful when things are not used
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- pattern matching on lists
-- WARNING: error function ==> throw an exception
-- WARNING: if we want to match several variables => parentheses
head' :: [a] -> a
head' [] = error "Cannot take the head of an empty list!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one item : " ++ show x
tell [x, y] = "The list has two items : " ++ show x ++ " and " ++ show y
tell (x:_) = "The list is too long. Here is the first item : " ++ show x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- 'as patterns' : permit to refer to the whole pattern without typing it entirely
capital :: String -> String
capital "" = "Empty string"
capital xs@(x:_) = "The first letter of " ++ xs ++ " is " ++ [x]

-- WARNING: no '++' in pattern match

-- guards: like if / else statements
-- syntax: cond = body to execute if cond is True
-- otherwise <=> else (otherwise = True)
-- WARNING: guards must be exhaustive
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| weight / height ^ 2 <= 18.5 = "You are underweight"
	| weight / height ^ 2 <= 25.0 = "You are normal"
	| weight / height ^ 2 <= 30.0 = "You are overweight"
	| otherwise = "You are obese"

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b = a
	| otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
-- We can define functions using the infix notation (so backticks)
a `compare'` b
	| a > b = GT
	| a == b = EQ
	| otherwise = LT

-- where statement : define things after using them
-- names visible in the guards
-- WARNING: must be properly aligned
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
	| bmi <= skinny = "You are underweight"
	| bmi <= normal = "You are normal"
	| bmi <= fat = "You are overweight"
	| otherwise = "You are obese"
	where bmi = weight / height ^ 2
	      skinny = 18.5
	      normal = 25.0
	      fat = 30.0

-- <-> initials (f:_) (l:_) = ...
initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
	where f = head first
	      l = head last

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2

-- let statement in ... : define variables and then use them in the current scope
-- WARNING: let is an expression <-> where are juste syntastic construct!
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
	let base = pi * r ^ 2
	    side = 2 * pi * r * h in
	2 * base + side

-- separate with semicolons
l = [let a = 100; b = 200 in a + b]

-- pattern matching with let bindings
tuple = (1, 2, 3, 4)
-- let (a, b, c, d) = tuple in a

-- in a list comprehension the let binding define name for the output function and the following predicates
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi w h | (w, h) <- xs, let bmi weight height = weight / height ^ 2, bmi w h > 25.0]

-- if we omit the 'in' part, then in ghci, the name will be defined for the entire session

-- case expressions : just pattern matching with parameters
-- case expression of pattern -> result
headBis :: [a] -> a
headBis xs = case xs of [] -> error "Empty list"
			(x:_) -> x

-- else : tested expression
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty"
					       [x] -> "a singleton"
					       [x, y] -> "a couple"
					       xs -> "a longer list"

