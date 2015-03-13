-- ignore warnings about missing implemented methods in typeclasses
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- allow to make instances of more complex types
{-# LANGUAGE FlexibleInstances #-}

-- HOMEWORK 6

-- ex 1
-- | Computes the Fibonacci numbers in a very bad way.
fib :: Integer -- ^ n
    -> Integer -- ^ F_n
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | The Fibonacci sequence (very bad way).
fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- ex 2
-- two solutions:
-- | The Fibonacci sequence (using zipWith).
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- | Computes the Fibonacci numbers using a tail-recursive function.
fib' :: Integer -- ^ n
     -> Integer -- ^ F_n
fib' n = fibaux 0 1 n
    where fibaux a b 0 = a
          fibaux a b n = fibaux (a + b) (a) (n - 1)

-- | The Fibonacci sequence (tail-recurive way).
fibs2' :: [Integer]
fibs2' = map fib' [0 .. ]

-- ex 3
-- | A 'Stream a' represents an infinite lists of 'a'.
data Stream a = Cons a (Stream a)

-- | Conversion between a stream and a list.
streamToList :: Stream a -- ^ A stream
             -> [a] -- ^ The corresponding list
streamToList (Cons x s) = x : streamToList s

-- | We just shows the first 20 elements of a stream.
instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- ex 4
-- | Constructs a stream containing the same element repeated infinitely.
streamRepeat :: a        -- ^ The element to repeat
             -> Stream a -- ^ The constructed stream
streamRepeat x = Cons x (streamRepeat x)

-- | Maps a function over each element of the stream.
streamMap :: (a -> b) -- ^ The function to map
          -> Stream a -- ^ The initial stream
          -> Stream b -- ^ The output stream
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

-- | Creates a stream from a seed.
streamFromSeed :: a        -- ^ The initial seed
               -> (a -> a) -- ^ The rule to get a new seed
               -> Stream a -- ^ The constructed stream
streamFromSeed seed f = Cons seed $ streamFromSeed (f seed) f

-- ex 5
-- | Stream of all natural numbers.
nats :: Stream Integer
nats = streamFromSeed 0 (+1)

-- | Interleaves two streams.
interleaveStreams :: Stream a -- ^ First stream
                  -> Stream a -- ^ Second stream
                  -> Stream a -- ^ Interleaved stream
interleaveStreams (Cons x1 s1) (Cons x2 s2) =
    Cons x1 $ Cons x2 $ interleaveStreams s1 s2

-- | The ruler function.
-- Odd numbers -> 0
-- Even numbers =
-- * 4 * n -> 2
-- * 4 * n + 2 -> 1
-- * Powers of 2
ruler :: Stream Integer
ruler = let a = streamRepeat 1
            b = streamRepeat 2
            c = streamFromSeed 3 (+1)
            d = streamRepeat 0
        in interleaveStreams d (interleaveStreams a (interleaveStreams b c))

-- ex 6
-- Polynomials are represented using infinite lists.
-- | 'x' represents the monomial x.
x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

-- | A polynomial (Stream Integer) is part of a ring.
instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate = streamMap negate
    (Cons x s) + (Cons x' s') = Cons (x + x') (s + s')
    (Cons x s) * b@(Cons x' s') = Cons (x * x') ((fromInteger x) * s' + s * b)

-- | We can also divide polynomials (in general, the result is not a polynomial).
instance Fractional (Stream Integer) where
    (Cons x s) / (Cons x' s') = q
        where q = Cons (x `div` x') (s - q * s')

-- | The Fibonacci sequence (using a generating function).
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- ex 7
-- | 2D Matrix data type
-- A matrix is represented as follow : [a b
--                                      c d]
data Matrix = Matrix {a :: Integer,
                      b :: Integer,
                      c :: Integer,
                      d :: Integer}
  deriving (Show)

-- | We can multiply matrices.
instance Num Matrix where
    m * m' = Matrix alpha beta gamma delta
        where alpha = (a m) * (a m') + (b m) * (c m')
              beta  = (a m) * (b m') + (b m) * (d m')
              gamma = (c m) * (a m') + (d m) * (c m')
              delta = (c m) * (b m') + (d m) * (d m')

-- (^) in Haskell already use the fast exponentiation algorithm
-- | The Fibonacci sequence (using matrices exponentiation which is very fast).
fib4 :: Integer
     -> Integer
fib4 0 = 0
fib4 n = b (f0 ^ n)
    where f0 = Matrix 1 1 1 0

