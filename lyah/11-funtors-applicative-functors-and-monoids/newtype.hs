module Main where

import Control.Applicative

-- newtype: make a type from an existing data type
-- ex: two ways for lists to be part of applicative:
-- -> apply functions like list comprehension
-- -> appply functions like zipping
-- BUT we can not define two typeclass instances of the same type
-- SOLUTION: create an other type that wraps inside a list and make
-- this type an instance of Applicative
-- We could do this by using data like this:
-- data ZipList a = ZipList {getZipList :: [a]}
-- We could also use 'newtype' and in fact it is more effective for cases like that
-- when we just want to wrap a type into a new type.
-- newtype ZipList a = ZipList {getZipList :: [a]}

-- newtype is limited to one value constructor and one field
-- we can of course derives instances
-- ex:
newtype CharList = CharList {getCharList :: [Char]} deriving (Show, Eq)

-- example of usage: make a 'difficult' type an instance
-- the pair (a, b) made an instance of Functor in the way that if we apply
-- fmap f then it will apply f to the first component of the pair
-- PROBLEM: a is the first type parameter
-- SOLUTION: create a new type by swapping the order of type parameters
newtype Pair b a = Pair {getPair :: (a, b)} deriving (Show)
instance Functor (Pair c) where
    fmap f (Pair (x, y)) = Pair (f x, y)

-- newtype is also LAZIER than data types
-- undefined is a special value in Haskell that means that an error
-- has occurred during the computation
-- >>> undefined -> exception
-- head [1, undefined] = 1 (because of laziness)
-- >>> data CoolBool = CoolBool {getCoolBool :: Bool}
-- >>> helloMe :: CoolBool -> String
-- >>> helloMe (CoolBool _) = "hello"
-- >>> helloMe undefined -> exception
-- For a data type, Haskell needs to evaluate just a little in order to
-- know which data constructor was used so it can pattern match correctly
-- When we try to evaluate undefined, Haskell evaluates it -> exception
-- Now, using a newtype:
newtype CoolBool = CoolBool {getCoolBool :: Bool}
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
-- >>> helloMe undefined = "hello"
-- In this cas, Haskell knows internally that a CoolBool value can only
-- be made from one constructor so it pattern matches and no exception is raised

-- IMPORTANT: pattern matching newtype is more like making a direct conversion
-- from one type to another

main :: IO ()
main = do
    return ()
