-- how to know the type of an expression with ghci ? ==> :t 'expr'
-- set +t: display type if 'it' after each instruction
-- '::' means 'has type of'
-- WARNING: in hs, types start with an uppercase letter

-- always give type declarations when writing functions
-- String is a synonym of [Char]
removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, elem c ['A' .. 'Z']]

add3 :: Integer -> Integer -> Integer -> Integer
add3 x y z = x + y + z

-- common types:
-- Int : bounded integer / Integer : not bounded (but less efficient)
-- Float : floating point with single precision / Double : double precision
circumference :: Double -> Double
circumference r = 2 * pi * r
-- Bool
-- Char
-- WARNING: empty tuple () has its own type

-- type variable : can be any type!
-- :t head ==> [a] -> a
-- a is a type variable
head' :: [a] -> a
head' l = head l

-- typeclasses : like interfaces in Java
-- if a type is a member of a typeclass, then it must implements some functions
-- WARNING: get the type of '+' ==> :t (+)
-- example: :t (==) ==> Eq a => a -> a -> Bool which means that '==' function takes two values and return a bool and its parameters are members of the 'Eq' class which is a class constraint

-- some typeclasses:
-- Eq : implement '==' and '/='
-- Ord : implement '>', '<', '>=' and '<=' and Eq
-- compare function return an Ordering (LT, GT, EQ)
a = 1 `compare` 2
-- Show : transform in a string -> show
b = show 3
-- Read : inverse of show ==> string -> a
c = read "4" :: Integer
-- here we must precise the result type as it is ambiguous
pif = pi :: Float
pid = pi :: Double
-- Enum : types that can be enumerated (Int, Integer, Double, Float, Bool, Char, Ordering, ()) ==> implement pred and succ
orders = [LT .. GT]
-- Bounded : types that have boundaries ==> minBound and maxBound ==> polymorphic constants
minInt = minBound :: Int
-- Num : numerics ==> implements Show and Eq
-- :t (+) ==> (Num t) => t -> t -> t
-- Integral : Int and Integer
-- Floating : Float and Double
-- fromIntegral : transform an Integral in a Num
l = fromIntegral (length [1, 2, 3]) + 1.1
