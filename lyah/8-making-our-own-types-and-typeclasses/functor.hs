-- the Functor typeclass should be defined for a type constructor (like Maybe)
-- ex: [] is the type constructor for lists
-- it defines only one function: fmap

-- functors can be used for types which can be used like a box : only ONE type parameter
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)
