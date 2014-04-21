module Person (
	Person(..),
	tellPerson) where

data WrongPerson = WrongPerson String String Int Float String String deriving (Show)

-- the record syntax : associate a name to a field and so create functions that associates an instance of this type to each field
data Person = Person {
		firstName :: String,
		lastName :: String,
		age :: Int,
		height :: Float,
		phoneNumber :: String,
		flavor :: String
} deriving (Eq, Show, Read)
-- here, Person is made a part of the 'Show' typeclass which means we can use the 'show' function on instances of this type
-- WARNING: each member of the value constructors have to be part of the typeclass we want to derive from
-- How does 'Eq' works : when we try to use '==', Haskell equates all pairs of parameters of the current value constructor

-- way to use records in functions
tellPerson :: Person -> String
tellPerson (Person {firstName = fn, lastName = ln, age = a, height = h, phoneNumber = pn, flavor = f}) = "Hey " ++ fn ++ " " ++ ln ++ ", you are " ++ show a ++ " years old and you measure " ++ show h ++ " m. Your phone number is " ++ pn ++ " and your favorite ice cream flavor is " ++ f
