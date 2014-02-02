-- set a more useful prompt
-- :set prompt "ghci> "

-- arihtmetic operations: +, -, *, / (type inference!)
-- WARNING: negative numbers -> surround with parentheses

-- boolean: True and False and operators : &&, ||, not
-- equality: == and /= (comparing string!)
-- WARNING: statically typed -> 5 == True ==> error!
-- type: integer, float, character, string, boolean

-- functions: *, + are infix functions
-- prefix: succ, min, max, div (integral division)
-- functions have the highest precedence
-- WARNING: if a function f takes two arguments: f a b <=> a `f` b

-- function defnition
doubleMe x = x + x
doubleUs x y = x*2 + y*2
doubleUs2 x y = doubleMe x + doubleMe y

-- if statement
-- WARNING: else part is MANDATORY
-- every function in Haskell must return something
-- then, an if statement is an expression
doubleSmallNumber x = if x > 100 then x else x * 2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
-- WARNING: ' is a valid character in Haskell (like a function name)
-- functions can NOT start with an uppercase letter

-- variables (definitions) are functions without parameters
a = 2
a' = 1

-- lists : set of same type objects
-- WARNING: 'let a = 1' in ghci is like 'a = 1' in a file
l = [1, 2, 3]
-- strings ARE lists of characters
s = "hello" ++ " " ++ ['w', 'o'] ++ "rld"
-- ++ is the append-to-list (list ++ list) operator and : prepend-to-list (var : list)
l' = 0 : l
-- !! is used to get and element in a list (start at 0)
c = "Hello" !! 0
-- WARNING: lists within lists HAVE to have the same type
-- comparison: < , > , <=, >=, ==, /= ==> lexicographical order

-- lists functions
-- head / tail
-- init / last
-- WARNING: empty lists!
-- length / null
-- reverse
-- take n l ==> take n elements from list l starting with the beginning of l /
-- drop n l ==> inverse of take
-- maximum / minimum
-- sum / product
-- elem a b ==> is a in b ?

-- ranges : easily create lists
-- create enumerated lists : numbers, characters (so string)
alphabet = ['a' .. 'z']
-- specify a step : type the two firsts!
even' = [2,4 .. 20]
-- reverse : specify a step
rev' = [20, 19 .. 1]
-- WARNING: use floats in range is not recommended

-- infinite ranges : not specify upper limit!
-- hs is lazy : it will not evalute directly the infinite list
-- functions: cycle, repeat, replicate + combine with take!

-- list by comprehension
-- [output function | variable in a set, that verify a predicate]
evenBis = [2 * x | x <- [0 .. 10]]
evenTerce = [x | x <- [0 .. 20], (x `mod` 2) == 0]
-- odd / even : tell if a number is odd or even
-- we can even work with mutliple lists
prod = [x * y | x <- [0..10], y <- [0..10], odd x, even y]
-- '_' : don't care about the elemnt we draw from the list
length' xs = sum [1 | _ <- xs]
upper xs = [c | c <- xs, elem c ['A' .. 'Z']]

-- nested lists
remove xxs = [ [x | x <- xs, even x] | xs <- xxs]

-- tuples : contain multiple variables with different types
-- tuples have a precise length
tuple = (1, 2, 'a', 'b', "hello", [1, 2])
lt = [(1, 'a'), (2, 'b')]
-- type! ==> each component in a list of tuples have to be the same type
-- WARNING: no singleton tuple

-- can be compared (only if the same size)
-- pairs : fst, snd
pair = (1, 2)
-- zip function : take two lists and make a list of pairs
alphabet' = zip [1..] ['a' .. 'z']

-- pb : find right triangles that has integers for all sides and sides have to be lower than 10 and have a perimeter of 24 ?
triangles = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]
rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
rightTriangles' = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]
