import Control.Monad ( when )

putStr' :: String -> IO ()
putStr' "" = return ()
putStr' (x:xs) = do
    putChar x
    putStr xs

main = do
    -- putStrLn
    putStrLn $ "putStrLn prints a string and a newline"

    -- putStr
    putStr $ "putStr prints a strign without jumping to a newline at the end"
    putStrLn $  ""

    -- putChar
    putStr $ "putChar puts a simple character on the screen : "
    putChar 'a'
    putStrLn $ ""

    -- print
    putStr $ "print puts a thing that can be shown on the screen : "
    print $ 1
    print $ "salut with quotes"

    -- getChar
    c <- getChar
    -- when is very useful
    -- it takes a predicate and an action
    -- when the predicate is false it returns unit
    when (c /= ' ') $ do
        putChar c
        main

    -- sequence
    -- a <- getLine
    -- b <- getLine
    -- c <- getLine
    -- is the same as :
    rs <- sequence [getLine, getLine, getLine]
    print rs

    -- it is common to sequence printing
    -- ex: sequence $ map print [1 .. 5]
    -- it is the same as mapM print [1 .. 5]
    -- mapM_ is better (not print the results of the actions at the end)

    -- getContents does lazy IO : it reads standard input until ite encounters an EOF character -> useful when using pipes
    -- foo <- getContents
