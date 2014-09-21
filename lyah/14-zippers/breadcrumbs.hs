data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving Show

data Direction = L | R deriving Show

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

-- Idea: making functions for just going left or right and remember the directions
type Breadcrumbs' = [Direction]

goLeft' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goLeft' (Node _ l _, bs) = (l, L : bs)

goRight' :: (Tree a, Breadcrumbs') -> (Tree a, Breadcrumbs')
goRight' (Node _ _ r, bs) = (r, R : bs)

x -: f = f x
-- ex: (freeTree, []) -: goRight -: goLeft

-- PROBLEM: not enough information to go up
-- SOLUTION: store with the direction the path we did not take
-- for example L <-> right subtree + root
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show

type Breadcrumbs a = [Crumb a]

goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node x l r, bs) = (l, LeftCrumb x r: bs)

goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node x l r, bs) = (r, RightCrumb x l: bs)

goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x l : bs) = (Node x t l, bs)
goUp (t, RightCrumb x r : bs) = (Node x r t, bs)

-- a zipper a a tuple of a data structure and an other one, the last one represent
-- a way to focus on a part of the first one
type Zipper a = (Tree a, Breadcrumbs a)

-- | Changes the element the zipper has the focus.
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify _ (Empty, bs) = (Empty, bs)

-- | Replace a tree in the current focus.
attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

-- | Goes all the way up.
topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

