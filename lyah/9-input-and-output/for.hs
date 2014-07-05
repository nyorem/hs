module Main where

import Control.Monad ( forM, forever )
import Data.Char ( toUpper )

main = do
    putStrLn "forM"
    -- forM is like mapM but the arguments are switched
    colours <- forM [1 .. 4] (\a -> do
        putStrLn $ "Which colour do you associate with the number " ++ show a ++ " ?"
        colour <- getLine
        return colour)
    mapM_ print colours

    -- forever indefinitly repeats the action
    putStrLn "forever"
    forever $ do
        input <- getLine
        putStrLn $ map toUpper input
