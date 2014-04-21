-- first example : data Bool = True | False
-- data means we define a new data type
-- the part after '=' are called value constructors
-- WARNING: type and value constructors have to be capitalized

import Shapes
import Person
import Vector

import qualified Data.Map as Map

-- simple data type
s1 = surface' (Circle' (Point 1 0) 1)

-- value constructors are functions so we can partially apply them
concentric = map (Circle' (Point 0 1)) [1..10]

circle = Circle' (Point 0 1) 1

-- instanciation of a record type
-- WARNING: we don't have to put the fields in the right order, we just have to list them all
guy = Person {firstName = "Jocelyn", lastName = "Meyron", age = 21, height = 1.9, phoneNumber = "0635509086", flavor = "Vanilla"}
pres = tellPerson guy

-- type parameters : like generic types, templates in C++
-- ex: data Maybe a = Nothing | Just a
-- Maybe is not a type : it is Maybe a which is
-- Nothing is Maybe a (just like [] is [a])
a = Just 10 :: Maybe Double
-- :t a -> a :: Maybe Double
-- WARNING: strong convention in hs = not add typeclass constraints in types declarations to save keystrokes (you have to put them in the functions declarations)

vect = Vector 1 1 1
s = (vect `vplus` vect) `vectMult` 2

-- instance of typeclasses
-- typeclasses are like interfaces that defined functions that can be used on the type we are defining
-- we can make a datatype part of a typeclass by using the 'deriving' keyword
guybis = Person {firstName = "jean", lastName = "jacques", age = 60, height = 2.0, phoneNumber = "12345678", flavor = "Chocolate"}
eq = guybis == guy

-- now, Person is part of the 'Eq' typeclass we can use all functions that work with this kind of datatype

-- to use the read function, we must specify the type we want to create
guyread = read "Person {firstName = \"jean\", lastName = \"jacques\", age = 60, height = 2.0, phoneNumber = \"12345678\", flavor = \"Chocolate\"}" :: Person

-- for the 'Ord' typeclass, the order is defined by the order we typed the value constructors : the first is the minimum
-- WARNING: an Ord must be an Eq
data Boolean = Faux | Vrai deriving (Eq, Ord, Show)
ret = Faux < Vrai -- True
-- if there are type parameters, then it will compare these two parameters (like in Maybe)

-- Enum, Bounded typeclasses
-- Enum : succ and pred works
-- Bounded : minBound and maxBound works
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturady | Sunday deriving (Show, Read, Eq, Ord, Bounded, Enum)
d = Monday
mini = minBound :: Day
days = [minBound .. maxBound] :: [Day]

-- Type synonyms
-- give a different name to sth : for ex, type String = [Char]
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

book :: PhoneBook
book = [("me", "0102030405")]

isInPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
isInPhoneBook name number book = (name, number) `elem` book

addPhoneBook :: Name -> PhoneNumber -> PhoneBook -> PhoneBook
addPhoneBook name  number book = (name, number) : book

-- type synonyms can be parametrized
type AssocList k v = [(k, v)]
-- isInAssocList :: (Eq k) => k -> AssocList k v -> Maybe v

-- type synonyms can be partially applied
type AssocInt v = AssocList Int v

-- WARNING: type constructors are ONLY used in the type portion of Haskell (data; type, ::)

-- datatype : Either (sth that represents an object that can be of two types)
-- data Either a b = Left a | Right b deriving (Show, Read, Eq, Ord)
-- used in errors handling, Left represent type of errors and results are in Right

-- example 1 : locker
-- each locker have a number, a code and a state
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
	case Map.lookup lockerNumber map of
		Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " does not exist!"
		Just (state, code) -> if state /= Taken
			then Right code
			else Left $ "The locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- recursive data structures
-- example 2 : list
-- we can call a constructor using an infix manner
-- WARNING: if we define a constructor using only special characters then it will automatically be infix
-- we can define the fixity of an operator using infixl (left associative), infixr( right associative) and infix (non-associative)
-- the level of precedence is in 0 to 9 (9 is the greatest)
infixr 5 :-:
data List a = EmptyList | a :-:  (List a) deriving (Show, Read, Eq, Ord)
list = 1 :-: 2 :-: EmptyList

infixr 4 .++
(.++) :: List a -> List a -> List a
EmptyList .++ l = l
(x :-: xs) .++ l = x :-: (xs .++ l)

-- example 3 : binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- insertion into a BST
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree  = singleton x
treeInsert x (Node y l r)
	| x == y = Node x l r
	| x > y = Node y l (treeInsert x r)
	| otherwise = Node y (treeInsert x l) r

tree = foldr treeInsert EmptyTree [1, 3, 2, 4, 10]

-- is our element in a BST ?
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node y l r)
	| x == y = True
	| x > y = treeElem x r
	| otherwise = treeElem x l

-- typeclasses : the come back
-- typeclasses = interfaces
-- we can just specify the prototypes and implement the functions later

data TrafficLight = Green | Orange | Red

-- instanciation of a typeclass : we define the beahviour of (==) (for Eq)
-- instance (typeclass) (type) where
-- (==) and (/=) are mutual recursive so we just need to define (==)
instance Eq TrafficLight where
	Red == Red = True
	Orange == Orange = True
	Green == Green = True
	_ == _ = False

instance Show TrafficLight where
	show Red = "Red light"
	show Orange = "Orange light"
	show Green = "Green light"

f = Red

-- you can add class constraints in instances declarations
-- ex: instance (Show a) => Maybe a where

-- WARNING: :info shows the functions thate we have to override when making an instance of a typeclass, the type of a function or the instances defined for a specific type
