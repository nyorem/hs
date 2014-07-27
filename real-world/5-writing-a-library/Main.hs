module Main where

import SimpleJSON

main :: IO ()
main = do
    print $ JObject [("foo", JNumber 1), ("bar", JBool False)]
