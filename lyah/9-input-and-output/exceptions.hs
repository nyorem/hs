module Main where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import System.IO.Error

-- some code can throw exceptions:
-- ex: pure functions => head [] or x `div` 0 or impure code => when opening a file, the file might not be present...

-- exceptions can only be handled in an IO code because we cannot know when a pure function will be called because of laziness (the pure code does not have a well-defined order of execution)
-- in general it is better to use Haskell's type system (like Maybe or Either) to handle error cases

-- code which can throw an exception
printNumberLines :: String -> IO ()
printNumberLines filename = do
    contents <- readFile filename
    putStrLn $ "The file has " ++ show (length (lines (contents))) ++ " lines!"

-- handler
-- we can use predicates to determine the type of the exception
-- we also can use functions to determine some attributes about the exception
handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e =
        case ioeGetFileName e of Just path -> putStrLn $ "The file does not exist at " ++ path ++ "!"
                                 Nothing -> putStrLn "File does not exist at unknown location!"
    | otherwise = ioError e -- we are not interested in the exception, we rethrow it

main :: IO ()
main = do
    args <- getArgs
    -- check the number of arguments
    if null args then do
        putStrLn "Not enough arguments!"
    else do
        let filename = head args
        -- check if the file exists : doesFileExist is a possibility but we will use exception here
        -- catchIOError takes an IO action (code to try) and the handler (IOError -> IO())
        (printNumberLines filename) `catchIOError` handler
