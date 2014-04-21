-- haskell type system is :
-- strong : not coerce everything, check usage of types (correct functions arguments)
-- static : compiler know the type of everything at compile time -> no type error during runtime
-- use type inference

-- function application have higher precedence!

-- one element tuple does not exist : (1) = 1
-- function application if left associative

-- we can cd through ghci using the ':cd' command!

-- WRONG!
-- x = 10
-- x = 11

-- simple function definition
myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop n (x:xs)
	| n <= 0 = []
	| otherwise = myDrop (n-1) xs

-- || is lazy (short circuit) => built-in because of the non-strict evaluation of Haskell

-- lazy evaluation : it reports the evaluation of thunks till it is really needed
-- the result of a function may be a thunk (ghci needs to evaluate it in order to prin t it on the screen)

-- type variables always start with a lowercase letter

-- '->' is right associative

lastButOne :: [a] -> a
lastButOne [] = error "lastButOne"
lastButOne [x] = error "lastButOne"
lastButOne (x:y:xs)
	| null xs = x
	| otherwise = lastButOne (y:xs)
