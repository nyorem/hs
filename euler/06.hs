-- PROJECT EULER : problem 6
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

sumSquares :: (Integral a) => a -> a
sumSquares n = (n * (n+1) * (2*n+1)) `div` 6

sum' :: (Integral a) => a -> a
sum' n = (n * (n+1)) `div` 2

res = (sum' 100)^2 - (sumSquares 100)

-- we also could finish the calculus and obtain en explicit formula for res(n)
