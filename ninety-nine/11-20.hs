-- problem 11 : modify run-length encoding so that if an element has no duplicates it is copied into the result list
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = packAux [] [x] x xs
	where packAux :: (Eq a) => [[a]] -> [a] -> a -> [a] -> [[a]]
	      packAux total acc e [] = reverse (acc:total)
	      packAux total acc e (x:xs)
		| x == e = packAux total (x:acc) e xs
		| otherwise = packAux (acc:total) [x] x xs

data ListItem a = Single a | Multiple Int a deriving (Show)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified l = map (\x -> if length x == 1 then Single (head x) else Multiple (length x) (head x)) (pack l)

-- problem 12 : decode a run-length encoding list via p11
decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap helper
	where helper (Single x) = [x]
	      helper (Multiple n x) = replicate n x

-- problem 13 : same as p11 but without using the 'pack' function
encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' 1 x xs
	where encodeDirect' n x [] = [encodeElement n x]
	      encodeDirect' n x (y:ys)
		| x == y = encodeDirect' (n+1) y ys
		| otherwise = encodeElement n x : encodeDirect' 1 y ys

encodeElement :: Int -> a -> ListItem a
encodeElement 1 x = Single x
encodeElement n x = (Multiple n x)

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


-- problem 19 : rotate a list n places to the left
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate l n
	| n == 0 = l
	| n < 0 = last pp : init pp
	| otherwise = tail p ++ [head p]
	where p = rotate l (n-1)
	      pp = rotate l (n+1)

rotate' :: [a] -> Int -> [a]
rotate' [] _ = []
rotate' l 0 = l
rotate' l@(x:xs) n
	| n > 0 = rotate' (xs ++ [x]) (n - 1)
	| otherwise = rotate' l (length l + n)

-- problem 20 : remove n'th element in a list
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "can not remove an element in an empty list"
removeAt n l = helper [] n l
	where helper :: [a] -> Int -> [a] -> (a, [a])
	      helper _ _ [] = error "invalid index"
	      helper acc n (x:xs)
		| n == 1 = (x, reverse acc ++ xs)
		| otherwise = helper (x:acc) (n-1) xs

removeAt' :: Int -> [a] -> (a, [a])
removeAt' n l = (l !! (n - 1), take (n - 1) l ++ drop n l)

removeAt'' :: Int -> [a] -> (a, [a])
removeAt'' 1 (x:xs) = (x, xs)
removeAt'' n (x:xs) = (l, x:r)
	where (l, r) = removeAt'' (n-1) xs
