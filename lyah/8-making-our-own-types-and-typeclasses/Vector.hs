-- type construtors can take type parameters (like template in C++) : way of making a type more generic
-- if there is a type parameter then we call the type a type construtor
-- ex: list definition is data List a = EmptyList | Cons a (List a) deriving (Show)

module Vector (
	Vector(..),
	vplus,
	vectMult) where

-- 3D vector representation
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

vectMult :: (Num a) => Vector a -> a -> Vector a
vectMult (Vector x y z) m = Vector (x*m) (y*m) (z*m)
