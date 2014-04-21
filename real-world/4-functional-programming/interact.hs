import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = unlines . splitLines

-- same semantic as lines but work more portably
splitLines :: String -> [String]
splitLines "" = []
splitLines cs =
    -- break reads the list till the predicate is false returning the two parts of the obtained list
    let (deb, fin) = break isLineTerminator cs in
        deb : case fin of
            ('\r' : '\n' : rest) -> splitLines rest
            ('\n' : rest) -> splitLines rest
            ('\r' : rest) -> splitLines rest
            _ -> []

-- check if a character is a line terminator : carriage return or newline
isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'
