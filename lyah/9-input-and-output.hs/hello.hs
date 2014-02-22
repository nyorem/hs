-- pb : in Haskell, we can not change the contents of a variable since it is a purely functional language (no side-effects) but how can it change the contents in the screen ?

-- hello world!
-- main is the entry point of the program
-- to build it : ghc --make hello.hs
main = putStrLn "Hello, World!"

-- :t putStrLn --> putStrLn :: String -> IO ()
-- putStrLn takes a string and returns an ACTION which result type is () (unit)
-- WARNING: when an action is performed, it will perform an action with a side-effect and will contain a return value inside of it
-- for example putStrLn does not need to return something
