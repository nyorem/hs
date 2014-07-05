module Main where

-- io management
import System.IO

main = do
    -- open a file read only, return the handle of the file which permits to manipulate it
    handle <- openFile "girlfriend.txt" ReadMode
    -- get the whole content of the file handled by 'handle'
    contents <- hGetContents handle
    putStr contents
    -- close the handle
    hClose handle

    putStrLn ""

    -- this is the same :
    withFile "girlfriend.txt" ReadMode (\h -> do
        contents <- hGetContents h
        putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    contents <- f handle
    hClose handle
    return contents
