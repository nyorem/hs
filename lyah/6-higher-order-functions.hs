-- higher order function : function that returns a function or that takes a function as a parameter
-- WARNING: every function in haskell can have only one parameter. We 'curried' multiple parameters functions
-- space means function application
-- ex: max is a function that takes an 'a' as a parameter and returns a function that takes an 'a' as parameter and returns an 'a'
-- if we call a function with less parameters, we obtain a partial function
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine = multThree 9

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- infix functions can also be partially applied -> surround with parentheses. It is called sections
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

uppercase :: Char -> Bool
uppercase = (`elem` ['A' .. 'Z'])

-- WARNING: if we want to use '-', we must use subtract function

-- WARNING: when we put things in ghci, it calculates the result first and then print it to the screen using the show function
-- functions are not part of the Show typeclass
