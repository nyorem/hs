{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n =
    fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 =
    map fib [ 0 .. ]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 =
    1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) =
    x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) =
        Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x =
    Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x =
    Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) s =
    Cons x (sInterleave s xs)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x xs) =
    x : sTake (n - 1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats =
    sIterate succ 0

ruler :: Stream Integer
ruler =
    let a = sRepeat 1
        b = sRepeat 2
        c = sIterate (+1) 3
        d = sRepeat 0
    in sInterleave d (sInterleave a (sInterleave b c))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed =
    sIterate (\x -> (1103515245 * x + 12345) `mod` 2147483648) seed

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 236 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 262 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) =
    Just $ foldr (\v (mm, m) -> let m' = if v > m then v else m
                                    mm' = if v < mm then v else mm
                                in (mm', m')) (x, x) xs

main :: IO ()
main =
    print $ minMaxSlow $ sTake 1000000 $ rand 7666532
-- main =
--     print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

newtype Matrix = Matrix { coeffs :: (Integer, Integer, Integer, Integer) }
               deriving Show

br :: Matrix -> Integer
br (Matrix (_, v, _, _)) = v

instance Num Matrix where
     Matrix (tl, tr, bl, br) + Matrix (tl', tr', bl', br') =
         Matrix (tl + tl', tr + tr', bl + bl', br + br')

     Matrix (tl, tr, bl, br) * Matrix (tl', tr', bl', br') =
         let tl'' = tl * tl' + tr * bl'
             tr'' = tl * tr' + tr * br'
             bl'' = bl * tl' + br * bl'
             br'' = bl * tr' + br * br'
         in Matrix (tl'', tr'', bl'', br'')

fastFib :: Int -> Integer
fastFib n =
    let f = Matrix (1, 1, 1, 0) in br (f ^ n)

