-- PROJECT EULER : problem 3
-- What is the largest prime factor of the number 600851475143 ?

-- Prime numbers decomposition
primeDivAux :: (Integral a) => a -> a -> a
primeDivAux n k
	| mod n k == 0 = k
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

-- the largest is the last
res = last (primeDecompo 600851475143)
