module Main where

import System.IO
import Data.Char ( toUpper )

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents

    -- writeFile deletes the file if it already exists
    writeFile "girlfriendCaps.txt" (map toUpper contents)

    -- appendFile works just as writeFile but appends contents to a file without deleting it if it already exists
    appendFile "girlfriendCaps.txt" "Hey!"

    -- change buffering mode with hSetBuffering
    withFile "girlfriend.txt" ReadMode (\handle -> do
        -- read by blocks of 2048 byts
        -- NoBuffering can be used to read character by character
        hSetBuffering handle $ BlockBuffering (Just 2048)
        contents <- hGetContents handle
        putStr contents)
