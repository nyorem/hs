{-# LANGUAGE FlexibleInstances, TypeSynonymInstances  #-}

module JoinListBuffer where

import Buffer
import JoinList
import Scrabble
import Sized

-- ex 4
-- Buffer instance for a JoinList (Score, Size) String.
-- The 'score' is the sum of the scrabble scores of the subtrees
-- The 'size' is the number of subtrees
instance Buffer (JoinList (Score, Size) String) where
    -- Convert a JoinList to a string.
    toString Empty = ""
    toString (Single _ x) = x
    toString (Append _ l r) = toString l ++ toString r

    -- Create a JoinList from a string.
    fromString = foldl (\acc str -> acc +++ (strToLine str)) Empty . lines

    -- Extracting the nth line is like indexJ.
    line = indexJ

    -- Replacing the nth line is like taking the first (n - 1) elements,
    -- adding the new line and then concatenate with the list where the first n
    -- elements were dropped.
    -- If the index is out of bounds, the buffer should be returned unmodified.
    replaceLine n l b
        | n > (getSize . size . tag $ b) = b
        | otherwise = t +++ fromString l +++ d
        where d = dropJ n b
              t = takeJ (n - 1) b

    -- The number of lines is contained in the second component of the root's tag.
    numLines = getSize . snd . tag

    -- The number of lines is contained in the first component of the root's tag.
    value = getScore . fst . tag

-- | Create a JoinList from a list of strings.
unlinesJoinList :: [String]                      -- ^ A list of strings
                -> JoinList (Score, Size) String -- ^ The corresponding JoinList
unlinesJoinList = fromString . unlines

