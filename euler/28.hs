-- PROJECT EULER : problem 28
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

-- The formula can be found considering a ring of size 2 * n + 1 (n is the level).
-- Its upper right corner is (2 * n + 1) ^2 and then we go backwards.
sumDiagonal :: Integer -> Integer
sumDiagonal 0 = 1
sumDiagonal n = 4 * (2 * n + 1) ^ 2 - 12 * n + sumDiagonal (n - 1)

res = sumDiagonal $ (1001 - 1) `div` 2

main :: IO ()
main = print res
