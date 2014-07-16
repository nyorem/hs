module Main where

import Party

-- | Reads a file representing a company and
-- computes the best associated GuestList.
main :: IO ()
main = do
    company <- readFile "company.txt"
    putStrLn . showGL . maxFun $ read company

