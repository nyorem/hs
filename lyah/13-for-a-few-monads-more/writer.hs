import Data.Monoid
import Control.Monad.Writer

-- part of 'mtl' package
-- Writer monad: logging system...
-- ex: attach to each value a string describing what we are doing : (a, String)
-- How can we chain these values ? If we have (a, str1) and (b, str2), how we can get (c, str3) ? We have a value with a context and a function that takes a value and returns a value with a context.
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (a, str) f = (b, str ++ log)
    where (b, log) = f a
-- ex: (3, "Smallish gang!") `applyLog` isBigGang

-- we can generalize applyLog to make the log message of any type m where m is a Monoid
betterApplyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
betterApplyLog (a, str) f = (b, str  `mappend` log)
    where (b, log) = f a

-- ex:
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)
-- now we can do: ("beans", 1) `betterApplyLog` addDrink

-- all of it can be done using the Writer monad:
-- newtype Writer w a = newtype {runWriter :: (a, w)}
-- a is the value and w is the monoidal one
-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer (x, mempty)
--     Writer (a, v) >>= f = let (b, w) = f a in (b, v `mappend` w)

-- do notation with Writer
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number " ++ show x])

mulWithLog :: Int -> Int -> Writer [String] Int
mulWithLog x y = do
    -- Writer w is a monad so
    -- in a statement 'a <- truc' a is the 'a' from Writer w 'a'
    a <- logNumber x
    b <- logNumber y

    -- Writer is part of MonadWriter typeclass
    -- so we can use 'tell' to add monoidal values at any point
    tell ["Gonna multiply " ++ show a ++ " and " ++ show b]
    return (a * b)

-- example of logging during the computation of gcd
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = do
        tell ["Finished with: " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcd' b (a `mod` b)

-- same example as above but construct the list of logs in the reverse order: it is
-- less efficient
-- above: log = a ++ (b ++ (c ++ d))
-- below: log = ((a ++ b) ++ c) ++ d
gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
    | b == 0 = do
        tell ["Finished with: " ++ show a]
        return a
    | otherwise = do
        result <- gcdReverse b (a `mod` b)
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        return result

-- good to use a data structure that is efficient when appending: difference lists
-- we represent a list ys by the following funtion: \xs -> ys ++ xs
-- then concatentation = function composition which is O(1)
newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

-- converting between difflists and lists
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Monoid (DiffList a) where
    mempty = DiffList ([] ++)
    (DiffList f)  `mappend` (DiffList g) = DiffList $ \xs -> f (g xs)

-- ex: fromDiffList (toDiffList [1, 2, 3] `mappend` toDiffList [4, 5, 6])
-- gcdReverse more efficient:
gcd'' :: Int -> Int -> Writer (DiffList String) Int
gcd'' a b
    | b == 0 = do
        tell (toDiffList ["Finished with: " ++ show a])
        return a
    | otherwise = do
        result <- gcd'' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

-- countdown in reverse
finalCountdown :: Int -> Writer (DiffList String) ()
finalCountdown 0 = do
    tell (toDiffList ["0"])
finalCountdown n = do
    finalCountdown (n - 1)
    tell (toDiffList [show n])

finalCountdown' :: Int -> Writer [String] ()
finalCountdown' 0 = do
    tell ["0"]
finalCountdown' n = do
    finalCountdown' (n - 1)
    tell [show n]

-- performance: finalCountdown > finalCountdown'

