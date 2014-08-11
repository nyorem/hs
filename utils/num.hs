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

-- fibonacci sequence with infinite lists
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibo :: Int -> Integer
fibo n = fibs !! n

-- Newton-Raphson algorithm to approximate square roots
next :: Double -> Double -> Double
next n x = (x + n / x) / 2

-- absolute error verification
abseps :: [Double] -> Double -> Double
abseps [] _ = error "list must have at least two elements"
abseps [x] _ = error "list must have at least two elements"
abseps (x:y:xs) eps
	| abs (x - y) < eps = y
	| otherwise = abseps (y:xs) eps

-- relative error verification
releps :: [Double] -> Double -> Double
releps [] _ = error "list must have at least two elements"
releps [x] _ = error "list must have at least two elements"
releps (x:y:xs) eps
	| abs (x - y) < eps * abs(y) = y
	| otherwise = releps (y:xs) eps

newtonabs :: Double -> Double -> Double -> Double
newtonabs n a0 eps = abseps (iterate (next n) a0) eps

newtonrel :: Double -> Double -> Double -> Double
newtonrel n a0 eps = releps (iterate (next n) a0) eps
