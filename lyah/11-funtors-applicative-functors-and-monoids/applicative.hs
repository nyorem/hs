module Main where

import Control.Applicative

-- Why do we need applicative functors ?
-- mapping a function which takes more than one parameter over a Functor
-- ex: fmap (*) (Just 3) wraps a function inside a Maybe : Num a => Maybe (a -> a)
-- now, we can just give parameters to the functions inside the functors
-- ex: fmap ($ 9) $ fmap (*) (Just 3) = Just 27
-- PROBLEM: how to give a parameter (which is inside a functor) to a function (which is inside a functor) ?
-- ex: let a = fmap (*) (Just 3)
--         b = Just 5
-- How to apply the '5' to the (* 3) ?
-- ANSWER: applicative functors.

main :: IO ()
main = do
    -- Applicative needs to be a Functor first
    -- it defines two methods: pure and <*>
    -- pure :: (Functor f) => a -> f a: wraps a value inside an applicative functor
    -- ex: :t pure 2 = (Functor f, Num a) => f a
    -- <*> :: (Functor f) => f (a -> b) -> (f a -> f b)
    -- takes a functor which has a function inside it and a functor and extracts the functio from the first functor and apply it to the second functor
    -- ex: Just (+ 2) <*> Just 4 = Just 6
    --     pure (+) <*> Just 2 <*> Just 3 = Just 5
    -- remark: pure f <*> x = fmap f x AND <*> is left associative!
    -- IMPORANT: names in type variables and in variables are not shared!

    -- Applicative also exports <$> as follows:
    -- f <$> x = fmap f x where f is a Functor
    -- then, we can just write f <$> x <*> y <*> z

    -- [] is also an instance of Applicative
    -- pure x = [x]
    -- fs <*> xs = [ f x | f <- fs, x <- xs ]
    -- ex: (++) <$> ["a", "b"] <*> ["c", "d"] = ["ac", "ad", "bc", "bd"]
    -- it is more 'beautiful' to use applicative style instead of list comprehensions
    -- ex: (*) <$> [2, 5, 10] <*> [8, 10, 11] == [ x * y | x <- [2, 5, 10], y <- [8, 10, 11] ]
    -- IMPORANT: we can view lists as non deterministic computations

    -- IO is an instance of Applicative
    -- pure = return
    -- a <*> b = do
    -- f <- a
    -- x <- b
    -- return (f x)
    -- ex:
    a <- (++) <$> getLine <*> getLine
    putStrLn a

    -- ((->) r) is an instance of Applicative (rarely used)
    -- pure x = (\_ -> x)
    -- f <*> g = \x -> (f x) (g x)
    -- ex: (+) <$> (+3) <*> (* 500) $ 5 == (5 + 3) + (5 * 500) = 2508
    -- (\x y z -> [x, y, z]) <$> (+2) <*> (*2) <*> (/2) $ 5 = [7.0, 10.0, 2.5]
    -- explanation: k <$> f <*> g : call k with the eventual results of f and g

    -- ZipList (part of Control.Applicative) : different way for a list to be part of Applicative
    -- ZipList : [f1, f2...] <*> [x1, x2...] = [f1 x1, f2 x2....]
    -- pure x = ZipList (repeat x) -- necesary because the use of zip (zip a finite list and an infinite list)
    -- ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
    -- getZipList is a function which extracts the list from a ZipList
    -- ex: getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [1, 2, 3]
    -- TIPS: (,) and (,,) funtion for creating pairs and triples

    return ()
