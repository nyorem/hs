module Main where

import           Data.List        (delete)
import           Data.Maybe
import           System.Directory (removeFile, renameFile)
import           System.IO        (IOMode (..), hClose, hFlush, hGetContents,
                                   hPutStr, openFile, openTempFile, stdout)

maybeReadInt :: String -> Maybe Int
maybeReadInt str =
    case reads str :: [(Int, String)] of
        [] -> Nothing
        [(x, _)] -> Just x

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"

    contents <- hGetContents handle
    let tasks = lines contents
        todosNumbered = unlines . (zipWith (\a b -> show a ++ " - " ++ b) [0 .. ]) $ tasks
    putStr $ "TODOS\n" ++ todosNumbered

    nstr <- prompt "Which one you want to delete ? "
    let maybeN = maybeReadInt nstr
    case maybeN of
        Nothing -> do
            removeFile tempName
            error "You didn't enter a number!"
        Just n -> do
            let newTodos = delete (tasks !! n) tasks

            hPutStr tempHandle $ unlines newTodos

            hClose handle
            hClose tempHandle

            removeFile "todo.txt"
            renameFile tempName "todo.txt"
