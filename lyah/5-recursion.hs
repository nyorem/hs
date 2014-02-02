-- some recursively functions implementations

-- maximum
-- basically
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs

-- using the max function
maximumBis' :: (Ord a) => [a] -> a
maximumBis' [] = error "Empty list"
maximumBis' [x] = x
maximumBis' (x:xs) = max x (maximumBis' xs)

-- replicate
-- WARNING: Num is not a subclass of Ord!
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x:(replicate' (n-1) x)

-- take
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

-- reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

-- repeat
repeat' :: a -> [a]
repeat' a = a:repeat' a

-- zip
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):(zip' xs ys)

-- elem
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
	| x == y = True
	| otherwise = elem' x ys

-- quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort lesser) ++ [x] ++ (quicksort greater)
		 where lesser = [y | y <- xs, y <= x]
		       greater = [y | y <- xs, y > x]
