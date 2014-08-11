-- functions are functors and fmap = (.)
-- functions are applicatives and :
-- instance Applicative ((->) r) where
--  pure :: a -> (r -> a)
--  pure = const
--
--  (<*>) :: r -> (a -> b) -> (r -> a) -> (r -> b)
--  f <*> g = \x -> f x (g x)
-- ex: (+) <$> (+2) <*> (* 10) $ 2 = 20 + 4

-- functions are also monads:
-- instance Monad ((->) r) where
--  return = const
--  (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
--  h >>= f = \x -> f (h x) x

-- ex:
addStuff :: Int -> Int
addStuff = do
    a <- (*2)
    b <- (+10)
    return (a + b)

-- all the functions in the block "read" from the same source -> Reader Monad

-- addStuff and addStuff' are the same
addStuff' :: Int -> Int
addStuff' x = let
    a = (*2) x
    b = (+10) x
    in a + b

