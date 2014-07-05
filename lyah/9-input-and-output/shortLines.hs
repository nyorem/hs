module Main where

shortLinesOnly :: String -> String
shortLinesOnly input =
        let l = lines input in
            unlines $ filter (\s -> length s < 10) l

-- when the program consists of applying a function to an input, we can use
-- the interact function

main = interact shortLinesOnly
-- shorter :
-- main = interact $ unlines . filter ((<10) . length) . lines
