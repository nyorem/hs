-- PROJECT EULER : problem 22
-- What is the total of all the name scores in the file "names.txt" ?
-- score = position of the name in the alphabetical order * sum alphabetical value of the name

module Main where

import Data.Char ( ord, toLower )
import Data.List ( sort )
import Data.List.Split ( splitOn )

-- the alphabetical value of a name
alphaValue :: String -> Int
alphaValue str = foldl (\acc c -> acc + alphaChar c) 0 (map toLower str)
    where alphaChar c = ord c - ord 'a' + 1

-- remove the first and the last element of a list
removeFL :: [a] -> [a]
removeFL = init . tail

main = do
    names <- readFile "names.txt"
    let namesSorted = sort $ map removeFL (splitOn "," names) in
        let namesNumbered = zip namesSorted [1 .. ] in
            print $ foldl (\acc (name, n) -> acc + n * alphaValue name) 0 namesNumbered
