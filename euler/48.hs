-- PROJECT EULER : problem 48
-- Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

sumSelf :: Integer -> Integer
sumSelf n = sum $ map (\x -> x ^ x) [ 1 .. n ]

res = reverse . take 10 . reverse . show $ sumSelf 1000
