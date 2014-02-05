-- problem 1 : find the last element in a list
myLast :: [a] -> a
myLast [] = error "can not retrieve last element in an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- problem 2 : find the last but one element in a list
myButLast :: [a] -> a
myButLast [] = error "can not retrieve last but one element in an empty list"
myButLast [_] = error "can not retrieve last but one element in a singleton list"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- problem 3 : find k'th element in a list (starting with 1)
elementAt :: Int -> [a] -> a
elementAt _ [] = error "index too large"
elementAt k (x:xs)
	| k < 1 = error "index too small"
	| k == 1 = x
	| otherwise = elementAt (k-1) xs

-- problem 4 : find the number of elements in a alist
myLength :: (Num i) => [a] -> i
myLength = foldl (\acc _ -> acc + 1) 0

-- problem 5 : reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldl (\acc x -> x : acc) []

-- problem 6 : test if a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' l = l == reverse l

-- problem 7 : flatten a nested list
-- we must define a datatype since Haskell lists are homogenous
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- problem 8 : elminate consecutive duplicates in a list
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = compressAux [x] x xs
	where compressAux acc e [] = reverse acc
	      compressAux acc e (x:xs)
		| x == e = compressAux acc e xs
		| otherwise = compressAux (x:acc) x xs

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : (compress $ dropWhile (== x) xs)

-- problem 9 : pack consecutive duplicates
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack (x:xs) = packAux [] [x] x xs
	where packAux :: (Eq a) => [[a]] -> [a] -> a -> [a] -> [[a]]
	      packAux total acc e [] = reverse (acc:total)
	      packAux total acc e (x:xs)
		| x == e = packAux total (x:acc) e xs
		| otherwise = packAux (acc:total) [x] x xs

-- problem 10 : length-encoding and decoding of a list
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode l = map (\x -> (length x, head x)) (pack l)

decode :: (Eq a) => [(Int, a)] -> [a]
decode [] = []
decode l = concatMap (\(a, b) -> replicate a b) l
