{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches c c' =
    length . filter (== True) $ zipWith (==) c c'

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors c =
    map (\x -> count x c) colors
        where count x = length . filter (== x)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c c' =
    sum $ zipWith min (countColors c) (countColors c')

inexactMatches :: Code -> Code -> Int
inexactMatches c c' = matches c c' - exactMatches c c'

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove c c' =
    Move c' (exactMatches c c') (inexactMatches c c')

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c e i) c' =
    exactMatches c c' == e && inexactMatches c c' == i

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes m cs =
    filter (isConsistent m) cs

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (\x -> [x]) colors
allCodes n =
    concatMap (\c -> map (c:) $ allCodes (n - 1)) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve c = go [initialMove]
    where initialMove = getMove c $ replicate (length c) Red
          codeLen = length c
          allCodesLen = allCodes codeLen
          isConsistentWithAll ms c' = all (`isConsistent` c') ms
          nextMove ms = getMove c $ head $ filter (isConsistentWithAll ms) $ allCodesLen
          go [] = []
          go mm@((Move _ e _):_)
            | e == codeLen = mm
            | otherwise = go (nextMove mm : mm)

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
