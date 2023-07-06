-- PROJECT EULER : problem 7
-- What is the 10 001st prime number?

-- Sieve of Eratosthenes
partialCrible :: (Integral a) => [a] -> a -> [a]
partialCrible xs k = [x | x <- xs, x == k || mod x k /= 0]

f :: (Integral a) => [a] -> a -> a -> [a]
f xs k n
	| k * k < n = f (partialCrible xs k)  (k + 1) n
	| otherwise = xs

crible :: (Integral a) => a -> [a]
crible n = f [2..n] 2 n

resBis = (crible 110000) !! (10001 - 1)

main :: IO ()
main = print resBis
