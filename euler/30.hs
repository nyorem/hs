-- PROJECT EULER : problem 30
-- Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

-- Upper bound: we need a x * 9^5 to five a x digits number, we find x = 6

module Main where

powerDigitsSum :: Integer -> Integer -> Integer
powerDigitsSum e n = sum . map (\c -> read [c] ^ e) $ show n

isEqualPowerDigit :: Integer -> Integer -> Bool
isEqualPowerDigit e n = n == powerDigitsSum e n

upper = 9 ^ 5 * 6

res = sum $ filter (isEqualPowerDigit 5) [ 2 .. upper ]

main :: IO ()
main = print res

