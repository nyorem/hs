-- PROJECT EULER : problem 2
-- By considering the terms in the Fibonacci sequence (starting with 1 and 2) whose values do not exceed four million, find the sum of the even-valued terms.

-- compute a list with all fibonacci numbers less or equal to b starting with x and y
fibaux :: Integer -> Integer -> Integer -> [Integer]
fibaux x y b
	| x <= b = x:rest
	| otherwise = []
	where rest = fibaux (x+y) x b

-- list of fibonacci sequence starting with 1 and 2 inferior to b
fib :: Integer -> [Integer]
fib b = fibaux 1 (2-1) b

-- we keep even numbers and we sum it!
res = sum (filter even (fib 4000000))
