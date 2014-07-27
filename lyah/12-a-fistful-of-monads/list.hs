module Main where

import Control.Monad

-- list monad: non determinism
-- lists as applicative: non determinsitic operations
-- (+) <$> [1, 2] <*> [10, 100, 1000]
-- instance Monad [] where
--    return x = [x]
--    xs >>= f = concat (map f xs)
--    fail _ = []
-- ex: [1, 2, 3] >>= \x -> [x, -x]
--     [1, 2] >>= \n -> ['a', 'b'] >>= \ch -> return (n, ch)
-- we can think about non-deterministic values interaction as building a tree where every possible result represents a branch

-- list comprehensions are syntactic sugar for using lists as monads
-- list comprehensions allow us to filter some elements, how can we do that using monads ?
-- MonadPlus (in Control.Monad) is a typeclass for monads that can also act as monoids (like Maybe, []...)
-- class Monad m => MonadPlus m where
--   mzero :: m a
--   mplus :: m a -> m a -> m a
-- mzero ~ mempty and mplus ~ mappend
-- then we can use the 'guard' function:
-- guard :: MonadPlus m => Bool -> m ()
-- guard False = mzero
-- guard True = return ()
-- example: guard (3 > 2) :: Maybe () = Just ()
--          guard (1 > 2) :: Maybe () = Nothing
--          guard (3 > 2) >> return "cool" = "cool"
--          guard (1 > 2) >> return "cool" = ""
-- so we can filter: [1 .. 50] >>= \x -> guard ('7' `elem` show x) >> return x

-- problem: given a chessboard and a knight, we ask ourselves if the knight can access a given case within three moves
-- we used the guard to keep the knight on the board
-- we can think at this like we proceed each element on a list at a time, checling if it is on the board or not
type KnightPos = (Int, Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1),
                 (c+1, r-1), (c+1, r+1), (c-1, r-1), (c-1, r+1)]
    guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
    return (c', r')
-- make three moves
in3 :: KnightPos -> [KnightPos]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
-- tell if a knight can go to this position
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start goal = goal `elem` in3 start

-- monad laws:
-- 1. Left identity: return x >>= f <-> f x
-- 2. Right identity: m >>= return <-> m
-- 3. Associativity: (m >>= f) >>= g <-> m >>= (\x -> f x >>= g)
-- --> composition of monadic functions:
--    (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
--    f <=< g = \x -> g x >>= f
--  then, the third law is: (f <=< g) <=< h <-> f <=< (g <=< h)

main :: IO ()
main = return ()

