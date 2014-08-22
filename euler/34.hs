-- PROJECT EULER : problem 34
-- Find the sum of all numbers which are equal to the sum of the factorial of their digits. (without 1 and 2)

-- Upper bound: we need a x * 9! to five a x digits number, we find x = 7

module Main where

import Data.Char ( digitToInt )

-- the factorial
fact :: Integer -> Integer
fact n = product [ 1 .. n ]

-- sum of the factorial of the digits
factorialDigitsSum :: Integer -> Integer
factorialDigitsSum n = sum . map (\x -> fact (read [x])) $ show n

upper = 7 * fact 9

res = sum $ filter (\x -> x == factorialDigitsSum x) [ 3 .. upper ]

main :: IO ()
main = print res

