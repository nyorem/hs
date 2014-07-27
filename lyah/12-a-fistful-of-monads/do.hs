module Main where

-- nested >>=:
-- Just 3 >>= \x -> Just "!" >>= (\y -> Just (show x ++ y))
-- reminds of: let x = 3; y = "!" in show x ++ y
-- WITHOUT do notation:
foo :: Maybe String
foo =
    Just 3 >>= (\x ->
    Just "!" >>= (\y ->
    Just $ show x ++ y))

-- WITH do notation:
foo2 :: Maybe String
foo2 = do
    x <- Just 3
    y <- Just "!"
    Just $ show x ++ y

-- explanation:
-- --> if any of the values extracted is Nothing then the whole do block evaluates to Nothing
-- --> every line must be a monadic value
-- --> last line must not be a binding (does not make sense)
-- --> result of a do block = last statement
-- --> 'x <- a' is equivalent to 'a >>= (\x -> ...)'
-- --> every statement is executed sequentially with its context
-- --> if we don't bind a value, those are equivalent:
-- >>> Nothing <-> >> Nothing <-> _ <- Nothing
-- --> when binding monadic values to names, we can use pattern matching. If the pattern matching fails, the 'fail' function is called (for example in a Maybe monad: fail = const Nothing)

-- example with Pierre (walk-the-line):
-- routine :: Maybe Pole
-- routine = do
--   start <- return (0, 0)
--   first <- landLeft 2 start
--   second <- landRight 2 first
--   landLeft 1 second

wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just "" -- pattern matching fails -> fail _ = Nothing
    return x

main :: IO ()
main = return ()

