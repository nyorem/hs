-- zippers allow to focus on a part of a data structure to make changes
-- easy and focusing efficient

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving Show

-- example of tree
freeTree :: Tree Char
freeTree =
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

-- if we want to change the 'W' by a 'P', we can say that 'W' is located in the
-- tree by first going right and then left
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)
-- this is very cumbersome

-- we can don better by making a function which takes a list of directions
data Direction = L | R deriving Show
type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' [] (Node _ l r) = Node 'P' l r
changeToP' (L : ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R : ds) (Node x l r) = Node x l (changeToP' ds r)
-- changeToP = changeToP' [R, L]

-- | Finds the element in the tree following the given directions.
elemAt :: Directions -> Tree a -> a
elemAt [] (Node x _ _) = x
elemAt (L : ds) (Node _ l _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r

-- next: breadcrumbs.hs
