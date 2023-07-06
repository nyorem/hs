-- PROJECT EULER : problem 23
-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

module Main where

-- list of proper divisors
divisors :: Int -> [Int]
divisors k = 1 : divisors' 2 k
    where divisors' n k
              | n*n > k = []
              | n*n == k = [n]
              | k `mod` n == 0 = (n: k `div` n : result)
              | otherwise = result
                  where result = divisors' (n + 1) k

-- detect if a number is abundant
isAbundant :: Int -> Bool
isAbundant n = sumDivisors n > n
    where sumDivisors = sum . divisors

-- all abundant numbers smaller than n
abundants :: Int -> [Int]
abundants n = filter isAbundant [ 1 .. (n - 1) ]

-- detect if a number cannot be written as a sum of 2 abundant numbers
isSumAbundants :: [Int] -> Int -> Bool
isSumAbundants ab n = go ab
    where go [] = False
          go (x:xs)
            | x >= n = False
            | isAbundant (n - x) = True
            | otherwise = go xs

ab = abundants 28123
res = sum $ filter (not . isSumAbundants ab) [ 1 .. 28123 ]

main :: IO ()
main = print res

