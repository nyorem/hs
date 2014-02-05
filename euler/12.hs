-- PROJECT EULER : problem 12
-- What is the value of the first triangle number to have over five hundred divisors?

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

-- calculating the number of divisors
-- first, we decompose n as prime products : n = p1^a1 * ... * pr^ar
-- then, we create the list [a1 .. ar]
-- finally, the number of divisors if (a1 + 1) * .. * (ar + 1)
keepSame :: (Eq a) => [a] -> Int
keepSame [] = 0
keepSame (x:xs) = keepSameAux x xs 1
	where keepSameAux _ [] acc = acc
	      keepSameAux x (y:ys) acc
		| y == x = keepSameAux x ys (acc + 1)
		| otherwise = acc

transfoDecompo :: (Eq a) => [a] -> [Int]
transfoDecompo [] = []
transfoDecompo (x:xs) = let r = keepSame (x:xs); right = snd (splitAt r (x:xs)) in
	r : transfoDecompo right

nbDivisors :: (Integral a) => a -> Int
nbDivisors n = let decompo = transfoDecompo (primeDecompo n) in
	product (map (+1) decompo)

-- generating triangle numbers
res = head (filter f (map g [1..]))
	where g x = (x * (x + 1)) `div` 2
	      f x = nbDivisors x >= 500
