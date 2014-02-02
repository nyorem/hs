-- PROJECT EULER : problem 1
-- Find the sum of all the multiples of 3 or 5 below 1000.

multiples :: (Integral a) => a -> [a]
multiples n = [x | x <- [1..(n-1)], x `mod` 3 == 0 || x `mod` 5 == 0]
res = sum (multiples 1000)
