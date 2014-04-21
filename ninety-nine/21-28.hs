-- problem 21 : insert an element at a specific position
insertAt :: a -> [a] -> Integer -> [a]
insertAt e [] 1 = [e]
insertAt _ [] _ = error "wrong index"
insertAt e (x:xs) n
	| n <= 0 = error "wrong index"
	| n == 1 = e:x:xs
	| otherwise = x : insertAt e xs (n - 1)

-- problem 22 : replicate haskell range with integers
range :: Integer -> Integer -> [Integer]
range down up
	| down == up = [up]
	| down <= up = down : range (down + 1) up
	| otherwise = down : range (down - 1) up

range' :: Int -> Int -> [Int]
range' down up = take (up - down + 1) $ iterate (+1) down

range'' :: (Ord a, Enum a) => a -> a -> [a]
range'' down up
	| down == up = [up]
	| otherwise = down : range'' ((if down <= up then succ else pred) down) up

-- problem 26 : generate k combinations from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = (combinations k xs) ++ (map (x :) $ combinations (k-1) xs)
