import Data.List

-- Data.List
-- intersperse : add an element in front of each element on a list
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' c (x:y:xs) = x : c : intersperse' c (y:xs)
l1 = intersperse ' ' "SALUT"

-- intercalate : same as intersperse except working with lists and flatten the result
l2 = intercalate " " ["je", "suis", "grand"]

-- transpose : transpose a list of lists (like a matrix)
l3 = transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- adding polynoms : transpose

-- foldl' and foldl1' : strict versions of foldl and foldl1 ==> compute values of the accumulator as they go along the list /= foldl and foldl1 which zre lazy

-- concat : flatten a list
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs
l4 = concat ["un", "deux", "trois"]

-- concatMap = concat . map
l5 = concatMap (replicate 4) [1..3]

-- and = take a list of booleans and returns the conjonction of all values
and' :: [Bool] -> Bool
and' (x:xs) = x && (and xs)
l6 = and $ map (>4) [1, 1, 4, 5]
-- there is also 'or'

-- any : take a predicate and tell if there is an element that satisfies the predicate
-- all : idem as any but all elements have to satisfy the predicate
any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = True
any' f (x:xs)
	| f x = True
	| otherwise = any' f xs

all' :: (a -> Bool) -> [a] -> Bool
all' f xs = and $ map f xs

-- iterate : build the infinite list with a starting value and a function to iterate on it
iterate' :: (a -> a) -> a -> [a]
iterate' f e = e : iterate' f (f e)
l7 = take 10 $ iterate (*2) 1

-- spliAt : split a list into to lists at a specific index

-- takeWhile : take a predicate and take elements in a list while the predicate is satisfied
-- WARNING: filter does not work on infnite lists whereas takeWhile works
l8 = sum $ takeWhile (<10000) $ map (^3) [1..]

-- dropWhile : drop the elements while the predicate is true
stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
l9 = head $ dropWhile (\(val, y, m, d) -> val < 1000) stock

-- span : like takeWhile except it returns a pair (takeWhile list, rest)
l10 = span (<20) [1, 3 .. 100]

-- break : like span but breaks when the predicate is first true
-- WARNING: break p = span (not . p)

-- sort : sort a list

-- group : take a list of adjacent duplicate elements and transform it to a list of lilists containing the duplicated element each time it is duplicated
l11 = group . sort $ [1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 1, 1]
l12 = map (\xs -> (head xs, length xs)) l11

-- inits / tails : apply recursively init and tail
l13 = inits "salut"
-- searching for a sublist : we could also use isInfixOf
search :: (Eq a) => [a] -> [a] -> Bool
search x y =
	let nlen = length x in
	foldl (\acc z -> if (take nlen z) == x then acc || True else acc) False (tails y)

-- search for a prefix : isPrefixOf or a suffix : isSuffixOf

-- notElem = not . elem

-- partition f xs = (l, r) where l contains all the elements of xs that satisfies f, r are the rest
l14 = partition (`elem` ['A' .. 'Z']) "SALUTjesuisHEY"

-- find : find the first element that satisfies the predicate
-- return Just a if found and Nothing if not
l15 = find (>1) [-1, 0, 2, 3, 3]
l16 = find (>4) [-1, 0, 2, 3, 3]
-- WARNING: find is much safer than drop-takeWhile...

-- elemIndex : find the index of the specified element in a list and nothing if not found
l17 = elemIndex 1 [2, 3, 1, 4, 6]

-- elemIndices : return a list of indices of the specified element

-- there are also findIndex and findIndices to find the indices of elements that satisfies a predicate

-- zip and zipWith with 3 .. 7 parameters
l18 = zip3 [1..10] [10..20] [20..30]
l19 = zipWith3 (\a b c -> a + b + c) [1..10] [10..20] [20..30]

-- lines / unlines : change or unchange "str\nstr" into ["str", "str"]
l20 = unlines $ lines "Salut\nComment ca va ?\nMoi ca va."
-- words / unwords : like lines but work with words

-- delete : delete the FIRST occurence of an element from a list
l21 = delete 'a' "salut"

-- work with sets : intersect / union / \\
-- WARNING: a \\ b : delete any element from b in a

-- insert : insert an element in an a list of elements that can be sorted in the first position where the next element is greater than the current one

-- generic functions (take or returns a Num instead of Int) : genericLength, genericTake, genericDrop, genericSplitAt, genericReplicate, genericIndex

-- it also exists 'By' functions (binay functions that returns a boolean) for : nub / delete / union / intersect / group
-- for example group = groupBy (==)

values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]
l22 = groupBy (\x y -> (x > 0) == (y > 0)) values

-- simplify by using the Data.Function.on function
-- on f g = \x y -> f (g x) (g y)
-- f is binay and g is unary
-- so, (x > 0) == (y > 0) <-> (==) `on` (> 0)

-- there are also : sortBy, minimumBy, maximumBy, insertBy
-- sort = sortBy compare
-- Idea : sortBy (compare `on` length)
