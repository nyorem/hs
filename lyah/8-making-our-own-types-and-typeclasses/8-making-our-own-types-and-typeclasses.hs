-- first example : data Bool = True | False
-- data means we define a new data type
-- the part after '=' are called value constructors
-- WARNING: type and value constructors have to be capitalized

import Shapes
import Person
import Vector

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
