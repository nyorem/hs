-- problem 14 : duplicate elements in a list
dupli :: [a] -> [a]
dupli = concatMap $ replicate 2

-- problem 15 : replicate elements in a list a given number of times
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- problem 16 : drop every n'th elements in a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = dropEveryAux [] 1 xs
	where dropEveryAux :: [a] -> Int -> [a] -> [a]
	      dropEveryAux acc ind [] = reverse acc
	      dropEveryAux acc ind (y:ys)
		| ind `mod` n == 0 = dropEveryAux acc (ind + 1) ys
		| otherwise = dropEveryAux (y:acc) (ind + 1) ys

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = map fst $ filter (\(x, i) -> i `mod` n /= 0) $ zip xs [1..]

-- problem 17 : split a list into two lists (the length of the first is given)
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' l@(x:xs) n
	| n > 0 = (x:ys, zs)
	| otherwise = ([], l)
	where (ys, zs) = split' xs (n-1)

split'' :: [a] -> Int -> ([a], [a])
split'' xs n = splitAux [] xs
	where splitAux :: [a] ->[a] -> ([a], [a])
	      splitAux l [] = (reverse l, [])
	      splitAux l (y:ys)
		| length l == n = (reverse l, y:ys)
		| otherwise = splitAux (y:l) ys

-- problme 18 : extract a slice from a list
slice :: [a] -> Int -> Int -> [a]
slice xs down up = drop (down - 1) . take up $ xs

slice' :: [a] -> Int -> Int -> [a]
slice' [] _ _ = []
slice' (x:xs) down up
	| down <= 0 = slice' (x:xs) 1 up
	| otherwise = helper [] (x:xs) (down - 1) (up - down + 1)
	where helper :: [a] -> [a] -> Int -> Int -> [a]
	      helper acc [] down up = []
	      helper acc (x:xs) down up
		| down == 0 && up == 0 = reverse acc
		| down /= 0 = helper [] xs (down - 1) up
		| up /= 0 = helper (x:acc) xs down (up - 1)

slice'' :: [a] -> Int -> Int -> [a]
slice'' xs down up = map fst $ filter (\(_, x) -> x >= down && x <= up) $ zip xs [1..]
