module Main where

-- first, we saw Functor for things that can be mapped over --> fmap
-- then, we saw Applicative for functors where functions inside --> pure and <*> (a value + context)
-- monads: extension. The problem is: if I have a value of type 'm a' and a function whose signature is 'a -> m b', how I can apply the function to the value ?
-- we need a function like:
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b
-- monads = applicatives + (>>=) (called 'bind')

-- simple example: Maybe monad (value with a context of possible failure)
-- Functor: fmap (++ "!") (Just "salut")
-- Applicative: max <$> Just 2 <*> Just 1
-- Monad: (>>=) : Maybe a -> (a -> Maybe b) -> Maybe b
-- applyMaybe is (>>=)
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = f x
-- Voc: a value of type 'm a' is a monadic (or a fancy) value

-- Monad typeclass: even if it is not written, a monad is an applicative (ghc 7.10)
-- class Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> (a -> m b) -> m b
--
--  (>>) :: m a -> m b -> m b
--  x >> y = x >>= (\_ -> y)
--
--  fail :: String -> m a
--  fail = error
-- explanations:
--  --> return = same as pure = puts a value into a monadic context
--  --> (>>=) = function application in a monadic context
--  --> (>>) = then operator = like (*>): discards the first parameter
--  --> fail = used by Haskell to enable failure in a syntactic construct

-- Monad instance for Maybe:
-- instance Monad Maybe where
--    return = Just
--    Nothing >>= _ = Nothing
--    Just  >>= f = f x
--    fail _ = Nothing

main :: IO ()
main = return ()

