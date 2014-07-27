module Main where

-- example of usage of (>>=) in a Maybe context
-- Pierre, a pole and birds
-- --> Pierre is balanced if |leftBirds - rightBirds| <= 3
type Birds = Int
type Pole = (Birds, Birds) -- left / right birds

pierreFalls :: Birds -> Birds -> Bool
pierreFalls left right = abs (left - right) > 3

-- FIRST IDEA:
-- Moves n birds to the left part of the pole.
badLandLeft :: Birds -> Pole -> Pole
badLandLeft n (left, right) = (left + n, right)

-- Moves n birds to the right part of the pole.
badLandRight :: Birds -> Pole -> Pole
badLandRight n (left, right) = (left, right + n)

-- Function application in the reverse order.
-- Better in this case.
(-:) :: a -> (a -> b) -> b
x -: f = f x

-- PROBLEM:
-- (0, 0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
-- >>> (0, 2)
-- everything seems OK but at the third landing, Pierre should fall!
-- SOLUTION: transform the function into functions that can fail -> Maybe

-- GOOD IDEA:
-- Maybe moves n birds to the left part of the pole.
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | pierreFalls (left + n) (right) = Nothing
    | otherwise = Just (left + n, right)

-- Maybe moves n birds to the right part of the pole.
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | pierreFalls left (right + n) = Nothing
    | otherwise = Just (left, right + n)

-- in order to write some lands sequentially, we need to use the bind operator because
-- we need to write a function that takes a 'Maybe Pole' and a 'Pole -> Maybe Pole'
-- landLeft 1 (0, 0) >>= landLeft 2
-- return (0, 0) >>= landLeft 1 >>= landRight 2 >>= landRight 2
-- we could not do this in with the Applicative style: an applicative can not interact
-- with the outside (that is to say take a function like 'a -> m b') and here the
-- actions depend on each other and in order

-- Make Pierre falls.
banana :: Pole -> Maybe Pole
banana = const Nothing

-- A function that ingore its input in a monadic context is given by (>>)
-- m >> n = m >>= \_ -> n
-- in a Maybe context: if one of the values is Nothing then the result is Nothing
-- if both are Just then the first one is dicarded
-- so, banana is equivalent to (>> Nothing)

main :: IO ()
main = return ()

