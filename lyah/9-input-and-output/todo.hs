module Main where

import           Data.List          (delete)
import           System.Directory   (removeFile, renameFile)
import           System.Environment (getArgs)
import           System.IO          (IOMode (..), hClose, hFlush, hGetContents,
                                     hPutStr, openFile, openTempFile, stdout)

-- dispatch list
-- functions take arguments and preform IO actions
dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add),
             ("view", view),
             ("remove", remove),
             ("bump", bump)
           ]

-- error message
errorExit :: IO ()
errorExit = do
    putStrLn "Unknown action!"

-- maybe execute a command
maybeExecute :: Maybe ([String] -> IO ()) -> [String] -> IO ()
maybeExecute Nothing _ = errorExit
maybeExecute (Just action) args = action args

-- add a todo
add [filename, todoItem] = do
    appendFile filename (todoItem ++ "\n")
add _ = error "wrong usage of add"

-- bump (move to the top) a todo
bump [filename, index] = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle

    let tasks = lines contents
        n = read index :: Int
        nTodo = tasks !! n
        todosWithoutN = delete nTodo tasks
        newTodos = [nTodo] ++ todosWithoutN

    hPutStr tempHandle $ unlines newTodos

    hClose handle
    hClose tempHandle

    removeFile "todo.txt"
    renameFile tempName "todo.txt"

bump _ = error "wrong usage of bump"

-- view a todo file
view [filename] = do
    contents <- readFile filename
    let tasks = lines contents
        todosNumbered = unlines . (zipWith (\a b -> show a ++ " - " ++ b) [0 .. ]) $ tasks
    putStr $ "TODOS\n" ++ todosNumbered
view _ = error "wrong usage of view"

-- remove a todo
remove [filename, index] = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle

    let tasks = lines contents
        n = read index :: Int
        newTodos = delete (tasks !! n) tasks

    hPutStr tempHandle $ unlines newTodos

    hClose handle
    hClose tempHandle

    removeFile "todo.txt"
    renameFile tempName "todo.txt"
remove _ = error "wrong usage of remove"

main :: IO ()
main = do
    (command:args) <- getArgs
    let maybeAction = lookup command dispatch in
        maybeExecute maybeAction args

