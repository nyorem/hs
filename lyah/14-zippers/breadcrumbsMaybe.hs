-- We add more security here by using the Maybe monad

-- tree
data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving Show

-- direction
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

-- breadcrumbs
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving Show
type Breadcrumbs a = [Crumb a]

-- zipper
type Zipper a = (Tree a, Breadcrumbs a)

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r: bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l: bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x l : bs) = Just (Node x t l, bs)
goUp (t, RightCrumb x r : bs) = Just (Node x r t, bs)
goUp (_, []) = Nothing

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
topMost z =
    case goUp z of
        Nothing -> z
        Just z' -> topMost z'

-- ex: return (freeTree, []) >>= goLeft >>= goRight
