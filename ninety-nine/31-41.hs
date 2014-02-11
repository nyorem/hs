-- problem 31 : determine if a number if prime or not
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = let maxi = round (sqrt (fromIntegral n :: Float)) in verifDiv 2 maxi
	where verifDiv :: Integer -> Integer -> Bool
	      verifDiv k b
		| k <= b = if n `mod` k == 0 then False else verifDiv (k + 1) b
		| otherwise = True

-- problem 32 : determine the greatest common divisor of two numbers
myGcd :: Integer -> Integer -> Integer
myGcd a b
	| b == 0 = abs a
	| otherwise = myGcd b (a `mod` b)

-- problem 33 : check if two numbers are coprimes
coprime :: Integer -> Integer -> Bool
coprime a b = (myGcd a b) == 1

-- problem 34 : calcule the euler's totient function phi(n) = numbers of positive integers r (1 <= r <= n) that are coprime to n
totient :: Integer -> Integer
totient n = helper 0 1
	where helper :: Integer -> Integer -> Integer
	      helper acc k
		| k < n = if coprime k n then helper (acc + 1) (k + 1) else helper acc (k + 1)
		| otherwise = acc

totient' :: Integer -> Integer
totient' n = fromIntegral $ length $ filter (coprime n) [1..n-1]

-- problem 35 : calculate the prime decomposition of a positive integer
-- first, we compute a prime divisor
primeDivAux :: (Integral a) => a -> a -> a
primeDivAux n k
	| mod n k == 0 =  k
	| otherwise = primeDivAux n (k+1)

primeDiv :: (Integral a) => a -> a
primeDiv n
	| n == 1 = 1
	| otherwise = primeDivAux n 2

-- then we compute the prime decomposition
primeFactors :: (Integral a) => a -> [a]
primeFactors n
	| n == 1 = []
	| otherwise = prime : primeFactors (n `div` prime)
	where prime = primeDiv n

-- problem 36 : idem as p35 but construct the list as (prime, multiplicty)
primeFactorsMult :: (Integral a) => a -> [(a, Int)]
primeFactorsMult 1 = []
primeFactorsMult n = let decompo = primeFactors n; x = head decompo; xs = tail decompo in
	helper 1 x xs
	where helper :: (Integral a) => Int -> a -> [a] -> [(a, Int)]
	      helper n x [] = [(x, n)]
	      helper n x (y:ys)
		| y == x = helper (n+1) x ys
		| otherwise = (x, n) : helper 1 y ys

-- problem 37 : compute euler's totient function using the prime decomposition
totientImproved :: Integer -> Integer
totientImproved n = product [(p - 1) * p ^ (m - 1) | (p, m) <- primeFactorsMult n]
