module Main where

import Data.Monoid -- monoid
import qualified Data.Foldable as F -- foldable

-- monoid: binary function which serves as an associative intern law
--         neutral element
-- in Haskell, a monoid is typeclass which can be applied to only concrete types
-- it defines three functions:
-- -> mempty = neutral element
-- -> mappend = binary function
-- -> mconcat = foldr mappend mempty (reduces monoid values to a single one)
-- Monoid laws:
-- mempty `mappend` x = x
-- x `mappend` mempty = x
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Functor instance
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

-- Foldable instance
instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r
testTree = Node 5
             (Node 3
                 (Node 1 Empty Empty)
                 (Node 6 Empty Empty)
             )
             (Node 9
                 (Node 8 Empty Empty)
                 (Node 10 Empty Empty)
             )

main :: IO ()
main = do
    -- examples of monoids:
    -- lists: mempty = [] and mappend = (++)

    -- numbers: two ways : (0, (+)) and (1, (*))
    -- two monoids (and two types) : Sum and Product
    -- getProduct . concat . map Product == product

    -- booleans: two ways (False, (||)) and (True, (&&))
    -- two monoids: Any and All
    -- getAny . concat . map Any == or

    -- Ordering: it is like comparing two words alphabetically
    -- mempty = EQ
    -- LT `mappend` _ = LT
    -- GT `mappend` _ = GT
    -- EQ `mappend` y = y

    -- Maybe a: 3 ways
    -- 1. if a is part of a monoid:
    -- mempty = Nothing
    -- Nothing `mappend` m = m
    -- m `mappend` Nothing = m
    -- Just x `mappend` Just y = Just (x `mappend` y)
    -- 2. if a is not in a monoid: discard the second value -> First a
    -- mempty = First Nothing
    -- First Nothing `mappend` x = x
    -- First (Just x) `mappend` _ = First (Just x)
    -- ex: find the first Just value in a list:
    -- getFirst . mconcat . map First $ [Nothing, Just 2, Just 3]
    -- 3. idem as 2 but for discard the first value -> List a
    -- mempty = Last Nothing
    -- Last Nothing `mappend` x = x
    -- _ `mappend` Last (Just x) = Last (Just x)
    -- ex: find the last Just value in a list:
    -- getLast . mconcat . map Last $ [Nothing, Just 2, Just 3]

    -- Foldable: make fold work with other data structures (monoids in that case)
    -- foldr == F.foldr (for lists)
    -- F.foldr works with Maybe values (not very interesting because only one value)
    -- ex: F.foldr (*) 2 (Just 1)
    -- foldMap: reduce a Foldable to a monoid value using a function
    -- using folds on our Tree data structure
    -- F.foldl (+) 0 testTree
    -- F.foldl (*) 1 testTree
    -- getAny $ F.foldMap (\x -> Any $ x == 3) testTree: know if there is a value in the tree equal to 3
    -- F.foldMap (\x -> [x]) testTree: turns the tree into a list

    return ()

-- example of using the Ordering monoid
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a

-- we put the more important comparison in the top...
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend`
                     (vowels x `compare` vowels y) `mappend`
                     (x `compare` y)
    where vowels = length . filter (`elem` "aeiouy")
