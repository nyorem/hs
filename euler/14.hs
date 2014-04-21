-- PROJECT EULER : problem 14
-- Which starting number for the syracuse sequence, under one million, produces the longest chain?

module Main where

-- create the syracuse sequence
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n : chain (n `div` 2)
	| otherwise = n : chain (3 * n + 1)

f m = snd (maximum $ zip (map (length . chain) [1 .. m]) [1 .. m])

main = do
	putStrLn $ show $ f 1000000
