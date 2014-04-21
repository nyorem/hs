import Data.List (sortBy)

-- a book have a number, a name and a list of authors
data BookInfo = Book Int String [String] deriving (Show)
book = Book 1 "Millenium" ["Stieg Larsson"]

-- a magzine have also a number, a name and a list of authors
-- BUT it it different from a book because of the type
data MagazineInfo = Magazine Int String [String] deriving (Show)
mag = Magazine 1 "Figaro" ["Jacques"]

-- value constructor is a function!

type CustomerID = Int
type ReviewBody = String
data BookReview = BookReview BookInfo CustomerID ReviewBody deriving (Show)

-- algebraic data types (with '|')
type CreditNumber = String
type CreditHolder = String
type Address = [String]

data BillingInformation = CreditCard CreditHolder CreditNumber Address
            | CashOnDelivery
            | Invoice CustomerID
            deriving (Show)
bill = CreditCard "Me" "You don't want to know" ["1", "bis"]

data Cartesian2D a = Cartesian2D a a deriving (Eq, Show)
data Polar2D a = Polar2D a a deriving (Eq, Show)

-- pattern matching
-- order matters!
x :: Cartesian2D a -> a
x (Cartesian2D abs _) = abs

y :: Cartesian2D a -> a
y (Cartesian2D _ ord) = ord

-- nice : -fwarn-incomplete-patterns

-- record syntax
data Customer = Customer {
    customerId :: CustomerID,
    customerName :: String,
    customerAddress :: [String]
} deriving (Show)

customer = Customer 12 "Name" ["a", "b"]
-- using the record syntax, we can change the order of the fields
customerBis = Customer {
    customerName = "Name",
    customerId = 12,
    customerAddress = ["a", "b"]
}

-- recursive types
-- list type
data List a = Cons a (List a)
        | Nil
        deriving (Show)

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

-- binary tree type
data Tree a = Node a (Tree a) (Tree a)
        | Empty
        deriving (Show)

simpleTree = Node "parent" (Node "left leaf" Empty Empty) (Node "right leaf" Empty Empty)

-- binary tree type with Maybe
data MaybeTree a = MaybeNode a (Maybe (MaybeTree a)) (Maybe (MaybeTree a)) deriving (Show)

convertTreeToMaybe :: Tree a -> MaybeTree a
convertTreeToMaybe (Node x Empty Empty) = MaybeNode x (Nothing) (Nothing)
convertTreeToMaybe (Node x Empty r) = MaybeNode x (Nothing) (Just (convertTreeToMaybe r))
convertTreeToMaybe (Node x l Empty) = MaybeNode x (Just (convertTreeToMaybe l)) (Nothing)
convertTreeToMaybe (Node x l r) = MaybeNode x (Just (convertTreeToMaybe l)) (Just (convertTreeToMaybe r))

-- handling errors : error or Maybe
-- error terminate immediately the program!
-- more controlled : Maybe

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs
    | null (tail xs) = Nothing
    | otherwise = Just $ head (tail xs)

safeSecond' :: [a] -> Maybe a
safeSecond' (_:x:_) = Just x
safeSecond' _ = Nothing

-- let bindings
-- shadowing
-- compiler option : -fwarn-name-shadowing
foo = let x = 1 in
        (let x = "foo" in x, x)

-- where clauses : like let but after

-- whitespace has meaning
-- we could use {a = 1; b = 2} to define things

-- case expressions

-- a name can only appear once in patterns!

-- guards (guards follow patterns)

-- exercises
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mean :: [Double] -> Maybe Double
mean xs
    | l == 0.0 = Nothing
    | otherwise = Just (s / l)
        where l = (fromIntegral $ myLength xs) :: Double
              s = sum xs

palindrome :: [a] -> [a]
palindrome l = l ++ myReverse l []
    where myReverse :: [a] -> [a] -> [a]
          myReverse [] acc = acc
          myReverse (x:xs) acc = myReverse xs (x:acc)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

sortByLength :: (Ord a) => [[a]] -> [[a]]
sortByLength ls = sortBy (\xs ys -> length xs `compare` length ys) ls

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (xs:[]) = xs
intersperse c (xs:ys) = xs ++ [c] ++ (intersperse c ys)

height :: Tree a -> Int
height Empty = 0
height (Node x l r) = 1 + max (height l) (height r)

data Point2D a = Point2D {xx :: a, yy :: a} deriving (Eq, Show)
data Direction = LeftTurn | RightTurn | Collinear deriving (Eq, Show)

turn :: (Num a, Ord a) => Point2D a -> Point2D a -> Point2D a -> Direction
turn a b c
    | area > 0 = LeftTurn
    | area < 0 = RightTurn
    | otherwise = Collinear
    where area = (xx b - xx a) * (yy c - yy a) - (yy b - yy a) * (xx c - xx a)

a = Point2D 0.0 0.0
b = Point2D 1.0 0.0
c = Point2D 0.0 1.0
d = Point2D 0.5 0.5
e = Point2D 0.5 0.3
ll = [a, b, c, d, e]

listTurn :: (Num a, Ord a) => [Point2D a] -> [Direction]
listTurn (p1:p2:p3:ps) = (turn p1 p2 p3) : listTurn (p2:p3:ps)
listTurn _ = []

-- Graham 2D convex hull algorithm
findBottomLeft :: (Ord a) => [Point2D a] -> Point2D a
findBottomLeft [] = error "list empty"
findBottomLeft (p:ps) = findBottomLeftAux p ps
    where findBottomLeftAux acc [] = acc
          findBottomLeftAux acc (x:xs)
            | yy x < yy acc  && xx x < xx acc = findBottomLeftAux x xs
            | otherwise = findBottomLeftAux acc xs

isEqual :: (Eq a) => Point2D a -> Point2D a -> Bool
isEqual p q = (yy p == yy q) && (xx p == xx q)

slope :: (Eq a, Fractional a) => Point2D a -> Point2D a -> a
slope p q
    | isEqual p q  = 0
    | otherwise = (yy q - yy p) / (xx q - xx p)

polarAngle :: (Eq a, Floating a) => Point2D a -> Point2D a -> a
polarAngle orig p = atan $ slope orig p

sortByPolarAngle :: (Floating a, Ord a) => Point2D a -> [Point2D a] -> [Point2D a]
sortByPolarAngle orig = sortBy (\p q -> compare (polarAngle orig p) (polarAngle orig q))

graham :: (Floating a, Ord a ) => [Point2D a] -> [Point2D a]
graham ll = let b = findBottomLeft ll
                s = sortByPolarAngle b ll in
                hullify s
            where hullify :: (Num a, Ord a) => [Point2D a] -> [Point2D a]
                  hullify [] = []
                  hullify (p1:[]) = [p1]
                  hullify (p1:p2:[]) = p1 : [p2]
                  hullify (p1:p2:p3:ps)
                    | turn p1 p2 p3 == RightTurn = hullify (p1:p3:ps)
                    | otherwise = p1 : hullify (p2:p3:ps)
