-- (..) means export all values constructors
-- same as writing Shape'(Circle', Rectangle')
-- we can not export values constructors if we want more safety : users can only create objects of our types using auxiliary functions (-> Data.Map)
module Shapes (
	Point(..),
	Shape'(..),
	surface',
	nudge) where

-- example : shape representation
-- could be a circle (x, y, radius) or a rectangle (coord of left up, coord of bottom right)
-- we use deriving (Show) in order to print Shape values : we inherit from the Show typeclass
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- WARNING: value constructors are functions that return a value of type the type we are currently defining, there are not types

-- we can pattern match against constructors
surface :: Shape -> Float
surface (Circle _ _ r) = pi * (r ^ 2)
surface (Rectangle x1 y1 x2 y2) = abs (x1 - x2) * abs (y1 - y2)

-- we can use the same name for the type and the valur constructor
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) * abs (y1 - y2)

nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x + a) (y + b)) r
nudge (Rectangle' (Point x y) (Point xx yy)) a b = Rectangle' (Point (x + a) (y + b)) (Point (xx + a) (yy + b))

-- inconveninent of this technique : unreadable if there are lots of fields -> record syntax
