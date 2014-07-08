-- HOMEWORK 3

module Golf (
              skips,
              localMaxima,
              histogram
            )
where

import Data.List ( transpose )

-- ex 1 : skips
-- | Constructs a list which contains the sequence of nth element of the input list.
skip :: [a] -- ^ The input list
     -> Int -- ^ The elements whose index are multiple of this index
     -> [a] -- ^ The output list
skip [] _ = []
skip l n = map snd . filter (\(i, x) -> (i `mod` n) == 0) $ zip [ 1 .. ] l

-- | Constructs a list of lists where the nth element of the output
-- contains each n-th element of the input list.
skips :: [a]   -- ^ The input list
      -> [[a]] -- ^ The output list
skips l = map (skip l) [1 .. (length l)]

-- ex 2 : local maxima
-- | Makes groups of threes.
-- ex: makeThrees [1, 2, 3, 4] = [[1, 2, 3], [2, 3, 4]]
makeThrees :: [Integer]                     -- ^ The input list
           -> [(Integer, Integer, Integer)] -- ^ Contains all groups of threes
makeThrees (x:y:z:xs) = (x, y, z) : makeThrees (y:z:xs)
makeThrees _ = []

-- | Predicate that determines if the second element of the triple
-- is a local maxima.
isLocalMaxima :: (Integer, Integer, Integer) -- ^ 3 numbers
              -> Bool                        -- ^ True if the second number if a local maxima
isLocalMaxima (a, b, c) = (b >= a) && (b >= c)

-- | Determines the local maxima of a list of integers.
localMaxima :: [Integer] -- ^ The input list of numbers
            -> [Integer] -- ^ The list of all local maxima
localMaxima xs = foldr (\l@(_, b, _) acc ->
    if isLocalMaxima l then b : acc else acc) [] (makeThrees xs)

-- ex 3 : histogram
-- | Count the number of occurrences of a number in a list.
countOccurrences :: [Integer] -- ^ The input list
                 -> Integer   -- ^ The number to find the occurrences of
                 -> Int       -- ^ The number of occurrences
countOccurrences xs n = length $ filter (== n) xs

-- | Precondition: the list only contains number from 0 to 9
histogram :: [Integer] -- ^ The input list of integers
          -> String    -- ^ The string representing the histogram
histogram l =
    let occ = map (countOccurrences l) [0 .. 9]
        occ' = zip occ [ 0 .. ]
        maxocc = maximum occ
        columns = map (\(x, i) -> (replicate (maxocc - x) ' ') ++ (replicate x '*') ++ "=" ++ show i) occ' in
            unlines . transpose $ columns

