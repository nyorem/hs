-- PROJECT EULER : problem 16
-- What is the sum of the digits of the number 2^1000?

n = 2^1000 :: Integer

sumDigits :: String -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sumDigits xs) + (read [x] :: Integer);

res = sumDigits (show n)
