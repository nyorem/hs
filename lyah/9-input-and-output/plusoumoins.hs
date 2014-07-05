module Main where

import System.Random
import System.IO (stdout, hFlush)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

askForNumber :: Int -> IO ()
askForNumber randNumber = do
    line <- prompt "Enter a number : "
    if not . null $ line then
        let n = read line :: Int in
            case (compare n randNumber) of
                EQ -> putStrLn "You won!"
                LT -> do
                    putStrLn "Plus!"
                    askForNumber randNumber
                otherwise -> do
                    putStrLn "Minus!"
                    askForNumber randNumber
    else
        askForNumber randNumber

main :: IO ()
main = do
    -- generate a random number between 1 and 100
    g <- getStdGen
    let (randNumber, _) = randomR (1, 100) g :: (Int, StdGen) in
        askForNumber randNumber
