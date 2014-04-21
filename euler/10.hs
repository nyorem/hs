-- PROJECT EULER : problem 10
-- Find the sum of all the primes below two million.

module Main where

-- primality test
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = let maxi = round (sqrt (fromIntegral n :: Float)) in verifDiv 2 maxi
	where verifDiv :: Integer -> Integer -> Bool
	      verifDiv k b
		| k <= b = if n `mod` k == 0 then False else verifDiv (k + 1) b
		| otherwise = True

-- sum of the primes below than n
primesSum :: Integer -> Integer
primesSum n = primesSumAux 5 5 n
	where primesSumAux s k n
		| k >= n = s
		| otherwise = primesSumAux (if isPrime k then s + k else s) (k + 2) n

main = do
	putStrLn $ show $ primesSum 2000000
