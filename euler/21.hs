-- PROJECT EULER : problem 11
-- Find the sum of all the amicable numbers under 1000
-- (two numbers a and b are amicable iif d(a) = b et d(b) = a where d(n) = sum of the proper divisors of n (/= n)

module Main where

import qualified Data.IntMap as M

-- find proper divisors
-- if we find a divisor d <= sqrt(n) then n / d is also a divisor of n
divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = findDivisors [1] 2
    where findDivisors s k
            | k == (ceiling . sqrt . fromIntegral $ n) = s
            | otherwise = if n `mod` k == 0 then findDivisors (k : n `div` k : s) (k + 1) else findDivisors s (k + 1)

-- sum of the proper divisors
sumDivisors ::  Int -> Int
sumDivisors = sum . divisors

-- is (a, b) amisable numbers ?
isAmicable ::  M.IntMap Int -> Int -> Int -> Bool
isAmicable m a b = (a /= b) && (b == m M.! a) && (a == m M.! b)

-- compute all the sum of the divisors of the numbers under 10000
mapDivisors :: M.IntMap Int
mapDivisors =
        let n = [ (x, sumDivisors x) | x <- [1 .. 10000] ] in
            M.fromList n

numbers = [ (x, y) | x <- [1 .. 10000], y <- [1 .. x] ]

main =  do
    print $ foldl (\acc (a, b) -> if isAmicable mapDivisors a b then acc + a + b else acc) 0 numbers
