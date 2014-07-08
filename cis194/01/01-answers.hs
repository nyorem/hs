-- HOMEWORK 01

-- Validating Credit Card Numbers (Luhn's algoirhtm)
-- ex 1
-- other way to do it: toDigits = map (\c -> read [c] :: Int) show
-- | Convert a number to the list of its digits in the reverse order.
toDigitsRev :: Integer   -- ^ An integer
            -> [Integer] -- ^ List of its digits in the reverse order
toDigitsRev n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = r : toDigitsRev q
        where r = n `mod` 10
              q = n `div`10

-- | Convert a number to the list of its digits.
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- ex 2
-- other way to do it:
-- apply :: (a -> Bool) -> (a -> a) -> [a] -> [a]
-- apply p f = map (\x -> if p x then f x else x)
-- doubleEveryOther xs = reverse $ map snd $ apply (\(i, _) -> even i) (\(i, v) -> (i, 2 * v))  $ zip [1 .. ] (reverse xs)
-- | Double every element with odd indices on the list beginning from the right.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ doubleEveryOtherAux (reverse xs) 0
    where doubleEveryOtherAux :: [Integer] -> Integer -> [Integer]
          doubleEveryOtherAux [] _ = []
          doubleEveryOtherAux (y:ys) n =
              (if odd n then 2 * y else y) : doubleEveryOtherAux ys (n + 1)

-- ex 3
-- | Compute the sum of all numbers of the list
-- (if a number has more than two digits then it is replaced by the sum of its digits).
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (sum . toDigits $ x) + sumDigits xs

-- ex 4
-- | Luhn's algorithm : a credit card number `n` (http://en.wikipedia.org/wiki/Luhn_algorithm)
-- will be valid iff `validate n == True`.
validate :: Integer -> Bool
validate x = (s `mod` 10) == 0
    where s = sumDigits . doubleEveryOther . toDigits $ x

-- Hanoi's towers
type Peg = String
type Move = (Peg, Peg)
-- | Resolve the Hanoi's towers problem.
hanoi :: Integer -- ^ Number of discs
      -> Peg     -- ^ Label of the first peg
      -> Peg     -- ^ Label of the second peg
      -> Peg     -- ^ Label of the third peg
      -> [Move]  -- ^ List of moves to do to solve the Hanoi problem
hanoi 0 _ _ _ = []
hanoi n a b c =
-- hanoi n a b c: move n disks from a to b using c as a temporary storage
-- 1. move (n-1) disks from a to c using b as a temporary storage
-- 2. move the top disk from a to b
-- 3. move (n-1) disks from c to b using a as a temporary storage
    hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

