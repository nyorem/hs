module JoinList where

import Sized
import Scrabble

import Data.Monoid

-- Binary tree where each node is composed of an element (a)
-- and a monoidal annotation (m)
data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- ex 1
-- | Finds the root's annotation.
tag :: Monoid m =>
       JoinList m a -- ^ A JoinList
       -> m         -- ^ The root's annotation
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- | Concatenates two JoinLists.
(+++) :: Monoid m =>
         JoinList m a    -- ^ First list
         -> JoinList m a -- ^ Second list
         -> JoinList m a -- ^ Concatenated list
jl1 +++ jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

-- ex 2
-- | Maybe finds the JoinList corresponding to an index.
-- Each node contains an annotation about the size of the subtrees.
-- It must satisfies: (indexJ i jl) = (jlToList jl !!? i).
indexJ :: (Sized b, Monoid b) =>
          Int             -- ^ The index
          -> JoinList b a -- ^ The list to look in
          -> Maybe a      -- ^ Maybe the asked element
indexJ _ Empty = Nothing
indexJ i (Single s a)
    | i == 0 = Just a
    | otherwise = Nothing
indexJ i (Append s l r)
    | i >= getSize (size s) = Nothing
    | i < n = indexJ i l
    | otherwise = indexJ (i - n) r
        where n = getSize . size . tag $ l

-- | Safe way to find the element of a list corresponding to an index.
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- | Transforms a JoinList to a list ignoring the monoidal annotations.
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

-- | Drop the first n elements of a JoinList.
-- It must satisfies: jlToList (dropJ n jl) == drop n (jlToList jl).
dropJ :: (Sized b, Monoid b) =>
         Int             -- ^ Number of element to drop
         -> JoinList b a -- ^ Initial list
         -> JoinList b a -- ^ List with n elements dropped
dropJ _ Empty = Empty
dropJ n x@(Single s _)
    | n > getSize (size s) = Empty
    | otherwise = x
dropJ n (Append s l r)
    | n >= getSize (size s) = Empty
    | n < nl = (dropJ n l) +++ r
    | otherwise = dropJ (n - nl) r
        where nl = getSize . size . tag $ l

-- | Take the first n elements of a JoinList.
-- It must satisfies: jlToList (takeJ n jl) == take n (jlToList jl).
takeJ :: (Sized b, Monoid b) =>
         Int             -- ^ Number of elements to take
         -> JoinList b a -- ^ Initial list
         -> JoinList b a -- ^ List with the first n elements of the initial one
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ _ x@(Single s _) = x
takeJ n x@(Append s l r)
    | n >= getSize (size s) = x
    | n < nl = takeJ n l
    | otherwise = l +++ (takeJ (n - nl) r)
        where nl = getSize . size . tag $ l

-- ex 3
-- see Scrabble module
-- | JoinList represeting the Scrabble score of a string.
scoreLine :: String                -- ^ A string
          -> JoinList Score String -- ^ JoinList with the Scrabble score
scoreLine str = Single (scoreString str) str

-- | JoinList represeting the Scrabble score and the the size of a string.
strToLine :: String                        -- ^ A string
          -> JoinList (Score, Size) String -- ^ Corresponding JoinList
strToLine s = Single ((scoreString s),(Size 1)) s

