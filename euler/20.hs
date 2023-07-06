-- PROJECT EULER : problem 20
-- Find the sum of the digits in the number 100!

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

sumDigits :: String -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumDigits xs) + (read [x] :: Integer);

res = sumDigits (show (fact 100))

main :: IO ()
main = print res
