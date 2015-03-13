-- HOMEWORK 04

-- ex 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (\acc x -> if even x then (x - 2) * acc else acc) 1

fun1'' :: [Integer] -> Integer
fun1'' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
    where f n = if even n then n `div` 2 else 3 * n + 1

-- ex 2
-- the Integer represent the height of the current node
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

-- | Create a balanced tree from a list.
foldTree :: [a]    -- ^ The input list
         -> Tree a -- ^ The built tree
foldTree = foldr insertNode Leaf

-- | Insert a node in a tree by conservating the tree balanced.
insertNode :: a      -- ^ The node to insert
           -> Tree a -- ^ The tree to insert the node in
           -> Tree a -- ^ The new tree
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node h ln xn rn)
    | treeHeight ln < treeHeight rn =
        let nn = insertNode x ln in Node (h + 1) nn xn rn
    | otherwise =
        let nn = insertNode x rn in Node (h + 1) ln xn nn

-- | Height of a node.
treeHeight :: Tree a  -- ^ Node
           -> Integer -- ^ Height of the node
treeHeight Leaf = 0
treeHeight (Node h _ _ _) = h

-- ex 3
-- | Computes the eXclusive OR of a list of booleans (True iff there is an
-- odd number of True values).
xor :: [Bool] -- ^ The input list
    -> Bool   -- ^ The resulting xor
xor = odd . foldl (\acc b -> if b then acc + 1 else acc) 0

-- | Version of Prelude.map using Prelude.foldr.
map' :: (a -> b) -- ^ The function to map
     -> [a]      -- ^ The input list
     -> [b]      -- ^ The output list where each element is the input list with the function mapped on
map' f = foldr (\x acc -> f x : acc) []

-- | foldl using foldr.
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- ex 4
-- | Computes the Sieve of Sundaram (http://en.wikipedia.org/wiki/Sieve_of_Sundaram).
-- For a given n, it computes all the odd prime numbers until 2 * n + 2.
sieveSundaram :: Integer   -- ^ n
              -> [Integer] -- ^ List of all odd prime numbers until 2 * N + 2
sieveSundaram n = map ((+ 1) . (* 2)) $ filter (\x -> x `notElem` composite) [1 .. n]
    where composite = [i + j + 2 * i * j | j <- [1 .. n],
                                           i <- [1 .. j],
                                           i + j + 2 * i * j <= n]

